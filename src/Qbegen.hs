{-# LANGUAGE OverloadedStrings #-}
module Qbegen(moduleToQbe, prettyMod) where

import Data.Text(Text, pack, isPrefixOf)
import Types
import Resolver(StructLookup, readType, literalType, gatherStructs, zipParameters)
import Data.IORef(IORef, modifyIORef', newIORef, readIORef)
import Control.Monad(when)
import Control.Monad.Trans.Reader(ReaderT, runReaderT, asks, local)
import Control.Monad.Trans.Class(lift)
import Data.Foldable(traverse_)
import Data.Int(Int32)
import Prettyprinter((<+>), Doc, parens)
import Helpers((<//>), find, indent, fromText, intercalate, escape, isConstructor)


-- Ident does not contain a sigil, while Val does
type Ident = Text

type Val = Text

type Ty = Text

data Mod = Mod Ident [Def]

data Block =
    Instruction Ident Ty Ident [Val]
    | CallInstruction Ident Ty Val [(Ty, Val)]
    | Store Ty Val Val
    | Label Ident
    | Jump Ident
    | JumpNonZero Val Ident Ident
    | Ret (Maybe Val)

data Def =
    DataDef Ident [(Ty, Val)]
    | TypeDef Ident [Ty]
    | FuncDef Ty Ident [(Ty, Ident)] [Block]

data Builder = Builder {
    -- List of global names and parameter names for determining sigil
    getGlobals :: [Text],
    getParameters :: [Text],
    getNameSupply :: IORef Int,
    getStringLiterals :: IORef [Def],
    getStructs :: StructLookup,
    -- Per function
    getAllocs :: IORef [Block],
    getInstructions :: IORef [Block]
}

newUnique :: ReaderT Builder IO Int
newUnique = do
    ref <- asks getNameSupply
    lift (modifyIORef' ref succ >> readIORef ref)

newIdent :: Text -> ReaderT Builder IO Text
newIdent prefix = fmap (\i -> prefix <> pack (show i)) newUnique

emit :: Block -> ReaderT Builder IO ()
emit instruction = do
    ref <- asks getInstructions
    lift (modifyIORef' ref (\instructions -> instructions ++ [instruction]))

emitAlloc name ty = do
    ref <- asks getAllocs
    size <- getSizeAsVal ty
    let instruction = Instruction name pointerTy ("alloc" <> getAlignmentAsText ty) [size]
    lift (modifyIORef' ref (\instructions -> instructions ++ [instruction]))

withParameters :: [Text] -> ReaderT Builder m a -> ReaderT Builder m a
withParameters params action =
    local (\(Builder globals _ uniques stringDefs structs instructions allocs) ->
        Builder globals params uniques stringDefs structs instructions allocs) action

withFreshBuilder :: ReaderT Builder IO () -> ReaderT Builder IO [Block]
withFreshBuilder action = do
    allocs <- lift (newIORef [])
    instructions <- lift (newIORef [])
    local (\(Builder params globals uniques stringDefs structs _ _) -> Builder params globals uniques stringDefs structs allocs instructions) action
    a <- lift (readIORef allocs)
    i <- lift (readIORef instructions)
    return (a ++ i)

isGlobal :: Text -> ReaderT Builder IO Bool
isGlobal name = do
    globalNames <- asks getGlobals
    return (elem name globalNames)

isParam :: Text -> ReaderT Builder IO Bool
isParam name = do
    params <- asks getParameters
    return (elem name params)

toBlock :: [Statement] -> ReaderT Builder IO ()
toBlock = traverse_ statementToBlock

bodyToBlock :: [Text] -> [Statement] -> ReaderT Builder IO ()
bodyToBlock parameters statements =
    withParameters parameters (toBlock statements)

statementToBlock :: Statement -> ReaderT Builder IO ()
statementToBlock (Return Nothing) =
    emit (Ret Nothing)
statementToBlock (Return (Just val)) = do
    returnVal <- expressionToVal' val
    emit (Ret (Just returnVal))
statementToBlock (Call expression) =
    expressionToVal expression >> return ()
statementToBlock (Assignment (Variable (Name name ty) _) rightHand) = do
    -- TODO handling complex leftHand expressions
    wasParam <- isParam name
    when wasParam (fail ("Parameter " ++ show name ++ " is constant and cannot be reassigned"))
    wasGlobal <- isGlobal name
    when wasGlobal (fail ("Global " ++ show name ++ " is constant and cannot be reassigned"))
    val <- expressionToVal' rightHand
    emit (Store (toQbeTy ty) val ("%" <> name))
statementToBlock (Definition (Name ident structType) (Apply (Variable constructor []) expressions)) | isConstructor constructor = do
    emitAlloc ident structType
    fields <- findFields structType
    zipParameters (emitField fields ident) fields expressions
    return ()
statementToBlock (Definition (Name name ty) expression) = do
    -- The allocation for locals are not emitted here, but per function.
    -- This is necessary so that we do not keep reallocating,
    -- for example in a while-loop
    emitAlloc name ty
    val <- expressionToVal' expression
    emit (Store (toQbeTy ty) val ("%" <> name))
statementToBlock (If [(condition, statements)] maybeElseBranch) = do
    thenLabel <- newLabel "then"
    elseLabel <- newLabel "else"
    endLabel <- newLabel "end"

    val <- expressionToVal' condition
    emit (JumpNonZero val thenLabel elseLabel)
    emit (Label thenLabel)
    toBlock statements
    emit (Jump endLabel)
    emit (Label elseLabel)
    toBlock (concat maybeElseBranch)
    emit (Label endLabel)
{-
@continue
    %cond = cond
    jnz %cond, @loop, @break
@loop
    sts1
    jmp @continue
@break
    ...
-}
statementToBlock (While condition statements) = do
    continueLabel <- newLabel "continue"
    loopLabel <- newLabel "loop"
    breakLabel <- newLabel "break"

    -- CONSIDER passing the continue and break label
    -- for implementing break and continue statements
    emit (Label continueLabel)
    val <- expressionToVal' condition
    emit (JumpNonZero val loopLabel breakLabel)
    emit (Label loopLabel)
    toBlock statements
    emit (Jump continueLabel)
    emit (Label breakLabel)
statementToBlock s = fail ("Cannot compile statement " ++ show s)

getSizeAsVal ty = do
    structEnv <- asks getStructs
    let size = getSize structEnv ty
    return (pack (show size))

getAlignmentAsText :: Type -> Text
getAlignmentAsText ty = pack (show (getAlignment ty))

getAlignment :: Type -> Int32
getAlignment (Concrete "char" []) = 1
getAlignment (Concrete "int" []) = 4
getAlignment (Concrete "long" []) = 8
getAlignment (Concrete "float" []) = 4
getAlignment (Concrete "double" []) = 8
getAlignment (Concrete "usize" []) = 8
getAlignment (PointerType _) = 8
-- TODO allow setting alignment of structs or getting default alignment,
-- e.g. qbe uses the max alginment of the fields
getAlignment _ = 4
--getAlignment other = error ("Cannot get alignment of " ++ show other)

findFields (Concrete structName []) = do
    structEnv <- asks getStructs
    find structName structEnv
findFields ty =
    fail ("Cannot get field of non struct type" ++ show ty)

getOffsetAsVal name fields = do
    offset <- getOffset name fields
    return (pack (show offset))

getOffset name [] = fail ("Did not encounter field " ++ show name)
getOffset name ((fieldName, ty):fields) =
    if name == fieldName
        then return 0
        else do
            structEnv <- asks getStructs
            let s = getSize structEnv ty
            o <- getOffset name fields
            return (s + o)

pointerSize :: Int32
pointerSize = 8

getSize _ (Concrete "char" []) = 1
getSize _ (Concrete "int" []) = 4
getSize _ (Concrete "long" []) = 8
getSize _ (Concrete "float" []) = 4
getSize _ (Concrete "double" []) = 8
getSize _ (Concrete "usize" []) = 8
getSize _ (PointerType _) = pointerSize
getSize structEnv (ArrayType ty (Just arraySize)) =
    let elementSize = getSize structEnv ty
    in elementSize * arraySize
getSize structEnv ty@(Concrete structName []) =
    case lookup structName structEnv of
        Nothing -> error ("Cannot determine size of type " ++ show ty ++ " because it was not in the environment" ++ show structEnv)
        Just fields ->
            let sizes = fmap (getSize structEnv . snd) fields
            in sum sizes
getSize _ other = error ("Cannot determine size of type " ++ show other)

expressionToVal' :: Expression -> ReaderT Builder IO Ident
expressionToVal' e = do
    (_, v) <- expressionToVal e
    return v

expressionToVal :: Expression -> ReaderT Builder IO (Ty, Val)
expressionToVal (Variable (Name name ty) []) = do
    let qbeTy = toQbeTy ty
    wasParam <- isParam name
    let wasStruct = isStructQbeTy qbeTy
    if wasParam || wasStruct
        then return (qbeTy, "%" <> name)
        else do
            freshIdent <- newIdent ".local"
            wasGlobal <- isGlobal name
            let sigil = if wasGlobal then "$" else "%"
            -- Local variables are either an address to the stack or global
            -- so a load instruction is necessary
            emit (Instruction freshIdent qbeTy ("load" <> qbeTy) [sigil <> name])
            return (qbeTy, "%" <> freshIdent)
expressionToVal (Literal l) = do
    val <- literalToVal l
    return (toQbeTy (literalType l), val)
expressionToVal (Apply (Variable n@(Name _ (FunctionType returnType _)) []) expressions) | isConstructor n =
    emitStruct returnType expressions
expressionToVal (Apply (Variable (Name "cast" _) [typeParameter]) [parameter]) = do
    freshIdent <- newIdent ".local"
    val <- expressionToVal' parameter
    case typeParameter of
        PointerType _ -> emit (Store pointerTy val ("%" <> freshIdent)) >> return (pointerTy, freshIdent)
        _ -> fail ("Conversion to " ++ show typeParameter ++ " not implemented yet")
expressionToVal (Apply (Variable (Name "sizeof" (FunctionType returnType _)) [typeParameter]) _) = do
    size <- getSizeAsVal typeParameter
    let qbeTy = toQbeTy returnType
    return (qbeTy, size)
expressionToVal (Apply (Variable (Name name (FunctionType returnType _)) []) expressions) = do
    freshIdent <- newIdent ".local"
    vals <- traverse expressionToVal expressions
    let qbeTy = toQbeTy returnType
    if isOperator name
        then emitOperator freshIdent qbeTy name vals
        else emit (CallInstruction freshIdent qbeTy ("$" <> name) vals)
    return (qbeTy, "%" <> freshIdent)
-- TODO Investigate: Does qbe allow calling local functions?
expressionToVal (Apply expression expressions) = do
    freshIdent <- newIdent ".local"
    vals <- traverse expressionToVal expressions
    (qbeTy, val) <- expressionToVal expression
    when (not (isPrefixOf "$" val)) (fail ("Is not a global function " ++ show val))
    emit (CallInstruction freshIdent qbeTy val vals)
    return (qbeTy, "%" <> freshIdent)
expressionToVal (DotAccess expression (Name fieldName fieldType) []) = do
    -- This will hold the offsetted pointer
    offsetted <- newIdent ".local"
    -- This will hold the read memory
    freshIdent <- newIdent ".local"
    val <- expressionToVal' expression
    let structType = readType expression
    fields <- findFields structType
    offset <- getOffsetAsVal fieldName fields
    emit (Instruction offsetted pointerTy "add" [val, offset])
    let qbeTy = toQbeTy fieldType
    if isStructQbeTy qbeTy
        then return (qbeTy, "%" <> offsetted)
        else emit (Instruction freshIdent qbeTy ("load" <> qbeTy) ["%" <> offsetted]) >> return (qbeTy, "%" <> freshIdent)

emitOperator :: Ident -> Ty -> Text -> [(Text, Val)] -> ReaderT Builder IO ()
emitOperator ident qbeTy "-_" [(_, val)] =
    emit (Instruction ident qbeTy "neg" [val])
emitOperator ident qbeTy "!" [(_, val)] =
    emit (Instruction ident qbeTy "ceqw" [val, "0"])
emitOperator ident qbeTy op [(qbeTy1, val1), (qbeTy2, val2)] =
    if qbeTy1 == qbeTy2
        then emitOperator' ident qbeTy op qbeTy1 val1 val2
        else fail ("Emit operator: Wrong type " ++ show qbeTy1 ++ " " ++ show qbeTy2)
emitOperator _ _ _ _ =
    fail "Emit operator: Wrong args"

emitOperator' :: Ident -> Ty -> Text -> Ident -> Val -> Val -> ReaderT Builder IO ()
emitOperator' ident qbeTy "+" _ val1 val2 =
    emit (Instruction ident qbeTy "add" [val1, val2])
emitOperator' ident qbeTy "-" _ val1 val2 =
    emit (Instruction ident qbeTy "sub" [val1, val2])
emitOperator' ident qbeTy "*" _ val1 val2 =
    emit (Instruction ident qbeTy "mul" [val1, val2])
emitOperator' ident qbeTy "/" _ val1 val2 =
    emit (Instruction ident qbeTy "div" [val1, val2])
emitOperator' ident qbeTy "==" argQbeTy val1 val2 =
    emit (Instruction ident qbeTy ("ceq" <> argQbeTy) [val1, val2])
emitOperator' ident qbeTy ">" argQbeTy val1 val2 =
    emit (Instruction ident qbeTy ("csgt" <> argQbeTy) [val1, val2])
emitOperator' ident qbeTy "<" argQbeTy val1 val2 =
    emit (Instruction ident qbeTy ("cslt" <> argQbeTy) [val1, val2])
emitOperator' ident qbeTy "<=" argQbeTy val1 val2 =
    emit (Instruction ident qbeTy ("csle" <> argQbeTy) [val1, val2])
emitOperator' ident qbeTy ">=" argQbeTy val1 val2 =
    emit (Instruction ident qbeTy ("csge" <> argQbeTy) [val1, val2])
emitOperator' ident qbeTy "!=" argQbeTy val1 val2 =
    emit (Instruction ident qbeTy ("cne" <> argQbeTy) [val1, val2])
emitOperator' _ _ op _ _ _ = fail ("Unknown operator " <> show op)

emitStruct structType expressions = do
    ident <- newIdent ".local"
    emitAlloc ident structType
    fields <- findFields structType
    zipParameters (emitField fields ident) fields expressions
    let qbeTy = toQbeTy structType
    return (qbeTy, "%" <> ident)

-- Nested struct creation:
-- When we have a struct Vector { int x, int y }
-- and struct Matrix { Vector v, Vector w }
-- and build it with Matrix(Vector(1, 2), Vector(3, 4))
-- then we don't want to allocate the Vectors, we just want to allocate Matrix
-- and assign the individual ints.
emitField fields ident (fieldName, fieldType) (Apply (Variable name []) expressions) | isConstructor name = do
    offsetted <- createOffset fields fieldName ("%" <> ident)
    nestedFields <- findFields fieldType
    zipParameters (emitField nestedFields offsetted) nestedFields expressions
    return ()
-- Nested struct copying:
-- Matrix(Vector2Int(x, y), makeVector2Int(z))
-- After creating a value, fields are copied one by one
-- this probably could be reused for assigning structs
emitField fields ident (fieldName, fieldType) expression | isStructQbeTy (toQbeTy fieldType) = do
    offsetted <- createOffset fields fieldName ("%" <> ident)
    nestedFields <- findFields fieldType
    (qbeTy, val) <- expressionToVal expression
    traverse_ (emitCopyField nestedFields ("%" <> offsetted) val) nestedFields
-- For base types
emitField fields ident (fieldName, fieldType) expression = do
    offsetted <- createOffset fields fieldName ("%" <> ident)
    (qbeTy, val) <- expressionToVal expression
    emit (Store qbeTy val ("%" <> offsetted))

isStructQbeTy = isPrefixOf ":"

createOffset fields fieldName val = do
    offsetted <- newIdent ".local"
    offset <- getOffsetAsVal fieldName fields
    emit (Instruction offsetted pointerTy "add" [val, offset])
    return offsetted

emitCopyField fields left right (fieldName, fieldType) = do
    leftOffsetted <- createOffset fields fieldName left
    rightOffsetted <- createOffset fields fieldName right
    let qbeTy = toQbeTy fieldType
    loaded <- newIdent ".local"
    emit (Instruction loaded qbeTy ("load" <> qbeTy) ["%" <> rightOffsetted])
    emit (Store qbeTy ("%" <> loaded) ("%" <> leftOffsetted))

literalToVal :: Literal -> ReaderT Builder IO Val
literalToVal (StringLiteral str) = registerConstant str
literalToVal l = return (literalToText l)

literalToText :: Literal -> Text
literalToText (Int32 l) = pack (show l)
literalToText (Int64 l) = pack (show l)
literalToText (Float32 l) = "s_" <> pack (show l)
literalToText (Float64 l) = "d_" <> pack (show l)
literalToText (Bool True) = "true"
literalToText (Bool False) = "false"
literalToText (StringLiteral _) =
    error "String literals have to be handled with literalToVal"

pointerTy :: Ty
pointerTy = "l"

voidTy :: Ty
voidTy = ""

toQbeTy :: Type -> Ty
toQbeTy (Concrete "void" []) = voidTy
toQbeTy (Concrete "char" []) = "b"
toQbeTy (Concrete "bool" []) = "w"
toQbeTy (Concrete "int" []) = "w"
toQbeTy (Concrete "long" []) = "l"
toQbeTy (Concrete "usize" []) = "uw"
toQbeTy (PointerType _) = pointerTy
-- TODO how to handle array types? always decay to pointer?
toQbeTy (ArrayType _ _) = pointerTy
toQbeTy (Concrete "auto" []) = error "Error: auto appeared in printing stage"
toQbeTy (Concrete s []) = ":" <> s
toQbeTy other =
    error ("Error: generic or function type " ++ show other ++ " appeared in printing stage")

newLabel :: Text -> ReaderT Builder IO Text
newLabel = newIdent

registerConstant :: Text -> ReaderT Builder IO Text
registerConstant str = do
    freshName <- newIdent "string"
    ref <- asks getStringLiterals
    let escaped = "\"" <> escape str <> "\\000\""
    lift (modifyIORef' ref (\c -> c ++ [DataDef freshName [("b", escaped)]]))
    return ("$" <> freshName)


-- Pretty Printing to Qbe format
moduleToQbe :: Module -> IO Mod
moduleToQbe (Module name statements) = do
    uniques <- newIORef 0
    stringDefs <- newIORef []
    instructions <- newIORef []
    allocs <- newIORef []
    let globalNames = gatherGlobalNames statements
    let structs = gatherStructs statements
    let baseBuilder = Builder globalNames [] uniques stringDefs structs instructions allocs
    let typeDefs = foldMap structToDef statements
    defs <- runReaderT (traverse statementToDef statements) baseBuilder
    createdStringDefs <- readIORef stringDefs
    return (Mod name (typeDefs ++ createdStringDefs ++ concat defs))

-- Top level
statementToDef :: Statement -> ReaderT Builder IO [Def]
statementToDef (Definition (Name name ty) (Literal l)) = do
    val <- literalToVal l
    return [DataDef name [(toQbeTy ty, val)]]
-- TODO ensure that a block for a function ends with a ret
-- we can't just check if the last block stat is a ret
statementToDef (FunctionDefintion name [] ty parameters statements) = do
    let qbeParams =fmap (\(Name n t) -> (toQbeTy t, n)) parameters
    blocks <- withFreshBuilder (bodyToBlock (fmap snd qbeParams) statements)
    return [FuncDef (toQbeTy ty) name qbeParams blocks]
statementToDef _ = return []

structToDef (StructDefinition ident [] fields) =
    [TypeDef ident (fmap (\(Name _ ty) -> toQbeTy ty) fields)]
structToDef _ = []

gatherGlobalNames :: [Statement] -> [Text]
gatherGlobalNames = foldMap gatherGlobalName

gatherGlobalName :: Statement -> [Text]
gatherGlobalName (Definition (Name name _) _) = [name]
gatherGlobalName (FunctionDefintion name _ _ _ _) = [name]
gatherGlobalName _ = []

prettyMod :: Mod -> Doc ann
prettyMod (Mod name defs) =
    fromText "# Compiled from" <+> fromText name
    <//> intercalate "\n\n" (fmap prettyDef defs)

-- For function definitions
prettyFunParam :: (Ty, Ident) -> Doc a
prettyFunParam (ty, ident) = prettyTy ty <+> "%" <> fromText ident

-- For calls
prettyParam :: (Ty, Val) -> Doc a
prettyParam (ty, val) = prettyTy ty <+> prettyVal val

-- For data definitions
prettyData :: (Ty, Val) -> Doc a
prettyData = prettyParam


prettyTy :: Ty -> Doc a
prettyTy ty = fromText ty

prettyVal :: Val -> Doc a
prettyVal val = fromText val

prettyDef :: Def -> Doc a
prettyDef (FuncDef ty ident parameters block) =
    "export function" <+> prettyTy ty <+> "$" <> fromText ident
    <> parens (intercalate ", " (fmap prettyFunParam parameters))
    <+> "{"
    <//> "@start"
    <//> intercalate "\n" (fmap prettyBlock block)
    <//> "}"
prettyDef (DataDef ident fields) =
    "export data" <+> "$" <> fromText ident <+> "=" <+> "{"
        <> intercalate ", " (fmap prettyData fields)
        <> "}"
prettyDef (TypeDef ident fields) =
    "type" <+> ":" <> fromText ident <+> "=" <+> "{"
        <> intercalate ", " (fmap prettyTy fields)
        <> "}"

prettyBlock :: Block -> Doc a
prettyBlock (Instruction ident ty instr parameters) =
    indent ("%" <> fromText ident <+> "=" <> prettyTy ty <+> fromText instr <+> intercalate ", " (fmap prettyVal parameters))
prettyBlock (CallInstruction ident ty f parameters) =
    indent ((if ty == voidTy
        then ""
        else "%" <> fromText ident <+> "=" <> prettyTy ty <+> "") <> "call" <+> fromText f <> parens (intercalate ", " (fmap prettyParam parameters)))
prettyBlock (Store ty val addressVal) =
    -- TODO ensure that ty is a base type
    indent ("store" <> fromText ty <+> prettyVal val <> "," <+> prettyVal addressVal)
prettyBlock (Label label) =
    -- Label is not indented
    "@" <> fromText label
prettyBlock (Jump label) =
    indent ("jmp" <+> "@" <> fromText label)
prettyBlock (JumpNonZero val thenLabel elseLabel) =
    indent ("jnz" <+> prettyVal val <> "," <+> "@" <> fromText thenLabel <> "," <+> "@" <> fromText elseLabel)
prettyBlock (Ret (Just val)) =
    indent ("ret" <+> prettyVal val)
prettyBlock (Ret Nothing) =
    indent "ret"
