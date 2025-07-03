{-# LANGUAGE OverloadedStrings #-}
module Qbegen(moduleToQbe, prettyMod) where

import Data.Text(Text, pack, isPrefixOf)
import Types
import Resolver(StructLookup, TypeLookup, readType, gatherStructs)
import Data.IORef(IORef, modifyIORef', newIORef, readIORef)
import Control.Monad(when, zipWithM_)
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

type Emit a = ReaderT Builder IO a

newUnique :: Emit Int
newUnique = do
    ref <- asks getNameSupply
    lift (modifyIORef' ref succ >> readIORef ref)

newIdent :: Text -> Emit Text
newIdent prefix = fmap (\i -> prefix <> pack (show i)) newUnique

emit :: Block -> Emit ()
emit instruction = do
    ref <- asks getInstructions
    lift (modifyIORef' ref (\instructions -> instructions ++ [instruction]))

emitAlloc :: Text -> Type -> Emit ()
emitAlloc name ty = do
    ref <- asks getAllocs
    size <- getSizeAsVal ty
    let instruction = Instruction name pointerTy ("alloc" <> getAlignmentAsText ty) [size]
    lift (modifyIORef' ref (\instructions -> instructions ++ [instruction]))

withParameters :: [Text] -> Emit a -> Emit a
withParameters params action =
    local (\(Builder globals _ uniques stringDefs structs instructions allocs) ->
        Builder globals params uniques stringDefs structs instructions allocs) action

withFreshBuilder :: Emit () -> Emit [Block]
withFreshBuilder action = do
    allocs <- lift (newIORef [])
    instructions <- lift (newIORef [])
    local (\(Builder params globals uniques stringDefs structs _ _) -> Builder params globals uniques stringDefs structs allocs instructions) action
    a <- lift (readIORef allocs)
    i <- lift (readIORef instructions)
    return (a ++ i)

isGlobal :: Text -> Emit Bool
isGlobal name = do
    globalNames <- asks getGlobals
    return (elem name globalNames)

isParam :: Text -> Emit Bool
isParam name = do
    params <- asks getParameters
    return (elem name params)

toBlock :: [Statement] -> Emit ()
toBlock = traverse_ statementToBlock

bodyToBlock :: [Text] -> [Statement] -> Emit ()
bodyToBlock parameters statements =
    withParameters parameters (toBlock statements)

statementToBlock :: Statement -> Emit ()
statementToBlock (Return Nothing) =
    emit (Ret Nothing)
statementToBlock (Return (Just val)) = do
    returnVal <- expressionToVal val
    emit (Ret (Just returnVal))
statementToBlock (Call expression) =
    expressionToVal expression >> return ()
statementToBlock (Assignment (Variable (Name name ty) _) rightHand) = do
    -- TODO handling complex leftHand expressions
    wasParam <- isParam name
    when wasParam (fail ("Parameter " ++ show name ++ " is constant and cannot be reassigned"))
    wasGlobal <- isGlobal name
    when wasGlobal (fail ("Global " ++ show name ++ " is constant and cannot be reassigned"))
    val <- expressionToVal rightHand
    emitStore ty val ("%" <> name)
statementToBlock (Definition (Name name structType) (Apply (Variable constructor []) expressions)) | isConstructor constructor = do
    -- This special case exists so that assigning new structs does not allocate twice,
    -- one alloc for the definition and one for the expression
    emitAlloc name structType
    emitAssignFields structType ("%" <> name) expressions
statementToBlock (Definition (Name name ty) expression) = do
    -- The allocation for locals are not emitted here, but per function.
    -- This is necessary so that we do not keep reallocating,
    -- for example in a while-loop
    emitAlloc name ty
    val <- expressionToVal expression
    emitStore ty val ("%" <> name)
statementToBlock (If branches maybeElseBranch) = do
    endLabel <- newLabel "end"
    emitBranches endLabel branches maybeElseBranch
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
    val <- expressionToVal condition
    emit (JumpNonZero val loopLabel breakLabel)
    emit (Label loopLabel)
    toBlock statements
    emit (Jump continueLabel)
    emit (Label breakLabel)
statementToBlock s = fail ("Cannot compile statement " ++ show s)

emitBranches :: Ident -> [(Expression, [Statement])] -> Maybe [Statement] -> ReaderT Builder IO ()
emitBranches endLabel [] maybeElseBranch = do
    toBlock (concat maybeElseBranch)
    emit (Label endLabel)
emitBranches endLabel ((condition, statements):rest) maybeElseBranch = do
    thenLabel <- newLabel "then"
    elseLabel <- newLabel "else"

    val <- expressionToVal condition
    emit (JumpNonZero val thenLabel elseLabel)
    emit (Label thenLabel)
    toBlock statements
    -- Only emit a jump if there is no ret
    -- It is a syntax error to have a jmp after a ret.
    if not (null statements) && isReturn (last statements)
        then return ()
        else emit (Jump endLabel)

    emit (Label elseLabel)
    emitBranches endLabel rest maybeElseBranch

getSizeAsVal :: Type -> Emit Text
getSizeAsVal ty = do
    structEnv <- asks getStructs
    let size = getSize structEnv ty
    return (pack (show size))

getAlignmentAsText :: Type -> Text
getAlignmentAsText ty = pack (show (getAlignment ty))

getAlignment :: Type -> Int32
getAlignment (Concrete "char" []) = 1
getAlignment (Concrete "short" []) = 2
getAlignment (Concrete "ushort" []) = 2
getAlignment (Concrete "int" []) = 4
getAlignment (Concrete "uint" []) = 4
getAlignment (Concrete "long" []) = 8
getAlignment (Concrete "ulong" []) = 8
getAlignment (Concrete "float" []) = 4
getAlignment (Concrete "double" []) = 8
getAlignment (Concrete "usize" []) = 8
getAlignment (PointerType _) = 8
-- TODO allow setting alignment of structs or getting default alignment,
-- e.g. qbe uses the max alginment of the fields
getAlignment _ = 4
--getAlignment other = error ("Cannot get alignment of " ++ show other)

findFields :: Type -> Emit TypeLookup
findFields (Concrete structName []) = do
    structEnv <- asks getStructs
    find structName structEnv
findFields (PointerType innerTy) =
    findFields innerTy
findFields ty =
    fail ("Cannot get field of non-struct type " ++ show ty)

getOffset :: Text -> TypeLookup -> Emit Int32
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

-- TODO does the return need to be Word64?
-- When are structs that big?
getSize :: StructLookup -> Type -> Int32
getSize _ (Concrete "bool" []) = 1
getSize _ (Concrete "char" []) = 1
getSize _ (Concrete "short" []) = 2
getSize _ (Concrete "ushort" []) = 2
getSize _ (Concrete "int" []) = 4
getSize _ (Concrete "uint" []) = 4
getSize _ (Concrete "long" []) = 8
getSize _ (Concrete "ulong" []) = 8
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

expressionToVal :: Expression -> Emit Val
expressionToVal (Variable (Name "nullptr" _) []) =
    return "0"
expressionToVal (Variable (Name name ty) []) = do
    wasParam <- isParam name
    wasGlobal <- isGlobal name
    let sigil = if wasGlobal then "$" else "%"
    if wasParam
        then return (sigil <> name)
        else emitLoadOrVal ty (sigil <> name)
expressionToVal (Literal l) =
    literalToVal l
expressionToVal (Apply (Variable n@(Name _ (FunctionType structType _)) []) expressions) | isConstructor n = do
    ident <- newIdent ".local"
    emitAlloc ident structType
    emitAssignFields structType ("%" <> ident) expressions
    return ("%" <> ident)
expressionToVal (Apply (Variable (Name "cast" _) [targetType]) [parameter]) = do
    let parameterType = readType parameter
    val <- expressionToVal parameter
    emitCast parameterType targetType val
expressionToVal (Apply (Variable (Name "sizeof" (FunctionType _ _)) [typeParameter]) _) =
    getSizeAsVal typeParameter
expressionToVal (Apply (Variable (Name name (FunctionType returnType parameterTypes)) []) expressions) = do
    freshIdent <- newIdent ".local"
    vals <- traverse expressionToVal expressions
    let retTy = toQbeWordTy returnType
    let parameterTys = fmap toQbeTy parameterTypes
    let zipped = zip parameterTys vals
    if isOperator name
        then emitOperator freshIdent retTy name zipped
        else do
            wasGlobal <- isGlobal name
            let sigil = if wasGlobal then "$" else "%"
            emit (CallInstruction freshIdent retTy (sigil <> name) zipped)
    return ("%" <> freshIdent)
-- TODO Should the following case be allowed? For example for currying?
-- Probably not very interesting without closuses, but possible
-- getCompare(compareOption)(a, b)
expressionToVal (Apply expression expressions) = do
    freshIdent <- newIdent ".local"
    val <- expressionToVal expression
    vals <- traverse expressionToVal expressions
    when (not (isPrefixOf "$" val)) (fail ("Is not a global function " ++ show val))
    let (FunctionType returnType parameterTypes) = readType expression
    let retTy = toQbeWordTy returnType
    let parameterTys = fmap toQbeTy parameterTypes
    let zipped = zip parameterTys vals
    emit (CallInstruction freshIdent retTy val zipped)
    return ("%" <> freshIdent)
expressionToVal (SquareAccess expression accessor) = do
    -- This will hold the accessor converted to size
    extVal <- newIdent ".local"
    -- This will hold the accessor times element size
    mulVal <- newIdent ".local"
    -- This will hold the offsetted pointer
    offsetVal <- newIdent ".local"
    val <- expressionToVal expression
    accessorVal <- expressionToVal accessor
    let (ArrayType elementType _) = readType expression
    elementSize <- getSizeAsVal elementType

    emit (Instruction extVal pointerTy "extsw" [accessorVal])
    emit (Instruction mulVal pointerTy "mul" ["%" <> extVal, elementSize])
    emit (Instruction offsetVal pointerTy "add" [val, "%" <> mulVal])

    emitLoadOrVal elementType ("%" <> offsetVal)
expressionToVal (DotAccess expression (Name fieldName fieldType) []) = do
    val <- expressionToVal expression
    let structType = readType expression
    fields <- findFields structType
    offset <- createOffset fields fieldName val
    emitLoadOrVal fieldType offset

emitOperator :: Ident -> Ty -> Text -> [(Text, Val)] -> Emit ()
emitOperator ident qbeTy "-_" [(_, val)] =
    emit (Instruction ident qbeTy "neg" [val])
emitOperator ident qbeTy "!" [(_, val)] =
    emit (Instruction ident qbeTy "ceqw" [val, "0"])
emitOperator ident qbeTy op [(qbeTy1, val1), (qbeTy2, val2)] =
    if qbeTy1 == qbeTy2
        then emitOperator' ident qbeTy op qbeTy1 val1 val2
        else fail ("Emit operator: Wrong type for operator " ++ show op ++ " " ++ show qbeTy1 ++ " " ++ show qbeTy2)
emitOperator _ _ _ _ =
    fail "Emit operator: Wrong args"

emitOperator' :: Ident -> Ty -> Text -> Ident -> Val -> Val -> Emit ()
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

-- Nested struct creation:
-- When we have a struct Vector { int x, int y }
-- and struct Matrix { Vector v, Vector w }
-- and build it with Matrix(Vector(1, 2), Vector(3, 4))
-- then we don't want to allocate the Vectors, we just want to allocate Matrix
-- and assign the individual ints.
emitField :: TypeLookup -> Val -> (Text, Type) -> Expression -> Emit ()
emitField fields var (fieldName, fieldType) (Apply (Variable name []) expressions) | isConstructor name = do
    offsetted <- createOffset fields fieldName var
    emitAssignFields fieldType offsetted expressions
-- Nested struct copying:
-- Matrix(Vector2Int(x, y), makeVector2Int(z))
-- After creating a value, fields are copied one by one
-- this probably could be reused for assigning structs
-- TODO there are many special cases for expression to val,
-- which does not alloc for structs. Maybe these special cases can be collapsed
emitField fields var (fieldName, fieldType) expression = do
    offsetVal <- createOffset fields fieldName var
    val <- expressionToVal expression
    emitStore fieldType val offsetVal

emitAssignFields :: Type -> Val -> [Expression] -> Emit ()
emitAssignFields structType val expressions = do
    fields <- findFields structType
    zipWithM_ (emitField fields val) fields expressions

createOffset :: TypeLookup -> Text -> Val -> Emit Val
createOffset fields fieldName val = do
    offset <- getOffset fieldName fields
    if offset == 0
        then return val
        else do
            offsetted <- newIdent ".local"
            let offsetVal = pack (show offset)
            emit (Instruction offsetted pointerTy "add" [val, offsetVal])
            return ("%" <> offsetted)

emitStore :: Type -> Val -> Val -> ReaderT Builder IO ()
emitStore ty val mem =
    if isPrimitive ty || isPointerType ty
        then emit (Store (toQbeStoreTy ty) val mem)
        else emitStoreFields ty val mem

emitStoreFields :: Type -> Val -> Val -> ReaderT Builder IO ()
emitStoreFields structType val mem = do
    nestedFields <- findFields structType
    traverse_ (emitStoreField nestedFields val mem) nestedFields

-- case for struct1.x = struct2.x
emitStoreField :: TypeLookup -> Val -> Val -> (Text, Type) -> Emit ()
emitStoreField fields val mem (fieldName, fieldType) = do
    valOffsetVal <- createOffset fields fieldName val
    memOffsetVal <- createOffset fields fieldName mem
    loadedVal <- emitLoadOrVal fieldType valOffsetVal
    emitStore fieldType loadedVal memOffsetVal

emitLoadOrVal :: Type -> Text -> ReaderT Builder IO Text
emitLoadOrVal ty val =
    if isPrimitive ty || isPointerType ty
        -- when accessing an int[] a with a[1] the result will be an int and loaded into a temporary
        then emitLoad ty val
        -- when accessing some someStruct[] a with a[1] the result will be a pointer to the struct, no load neccessary
        else return val

emitLoad :: Type -> Val -> ReaderT Builder IO Text
emitLoad ty val = do
    let broadQbeTy = toQbeWordTy ty
    let qbeTy = toQbeTy ty
    loaded <- newIdent ".local"
    emit (Instruction loaded broadQbeTy ("load" <> qbeTy) [val])
    if broadQbeTy == qbeTy
        then return ("%" <> loaded)
        else do
            extended <- newIdent ".local"
            emit (Instruction extended broadQbeTy ("ext" <> qbeTy) ["%" <> loaded])
            return ("%" <> extended)

-- TODO remove overlap by finding general rules
emitCast :: Type -> Type -> Text -> Emit Text
emitCast parameterType targetType val | parameterType == targetType = return val
emitCast (PointerType _) (PointerType _) val = return val
emitCast (Concrete "int" []) (Concrete "uint" []) val = return val
emitCast (Concrete "uint" []) (Concrete "int" []) val = return val
emitCast (Concrete "int" []) (Concrete "ushort" []) val = return val
emitCast (Concrete "int" []) (Concrete "short" []) val = return val
emitCast (Concrete "int" []) (Concrete "char" []) val = return val
emitCast (Concrete "ulong" []) (Concrete "int" []) val = return val
emitCast (Concrete "int" []) (Concrete "ulong" []) val = return val
emitCast (Concrete "long" []) (Concrete "int" []) val = return val
emitCast (Concrete "ulong" []) (Concrete "long" []) val = return val
emitCast (Concrete "long" []) (Concrete "ulong" []) val = return val
-- TODO these probably work on smaller types, e.g. char to long
emitCast (Concrete "uint" []) (Concrete "long" []) val = do
    freshIdent <- newIdent ".local"
    emit (Instruction freshIdent "l" "extuw" [val])
    return ("%" <> freshIdent)
emitCast (Concrete "int" []) (Concrete "long" []) val = do
    freshIdent <- newIdent ".local"
    emit (Instruction freshIdent "l" "extsw" [val])
    return ("%" <> freshIdent)
emitCast (Concrete "char" []) targetType val | isIntegerType targetType = do
    freshIdent <- newIdent ".local"
    emit (Instruction freshIdent (toQbeTy targetType) "extub" [val])
    return ("%" <> freshIdent)
emitCast (Concrete "ushort" []) targetType val | isIntegerType targetType = do
    freshIdent <- newIdent ".local"
    emit (Instruction freshIdent (toQbeTy targetType) "extuh" [val])
    return ("%" <> freshIdent)
emitCast (Concrete "short" []) targetType val | isIntegerType targetType = do
    freshIdent <- newIdent ".local"
    emit (Instruction freshIdent (toQbeTy targetType) "extsh" [val])
    return ("%" <> freshIdent)
emitCast (Concrete "int" []) targetType val | isFloatingType targetType = do
    freshIdent <- newIdent ".local"
    emit (Instruction freshIdent (toQbeTy targetType) "swtof" [val])
    return ("%" <> freshIdent)
emitCast (Concrete "uint" []) targetType val | isFloatingType targetType = do
    freshIdent <- newIdent ".local"
    emit (Instruction freshIdent (toQbeTy targetType) "uwtof" [val])
    return ("%" <> freshIdent)
emitCast (Concrete "long" []) targetType val | isFloatingType targetType = do
    freshIdent <- newIdent ".local"
    emit (Instruction freshIdent (toQbeTy targetType) "sltof" [val])
    return ("%" <> freshIdent)
emitCast (Concrete "ulong" []) targetType val | isFloatingType targetType = do
    freshIdent <- newIdent ".local"
    emit (Instruction freshIdent (toQbeTy targetType) "ultof" [val])
    return ("%" <> freshIdent)
emitCast (Concrete "float" []) (Concrete "double" []) val = do
    freshIdent <- newIdent ".local"
    emit (Instruction freshIdent "d" "exts" [val])
    return ("%" <> freshIdent)
emitCast (Concrete "double" []) (Concrete "float" []) val = do
    freshIdent <- newIdent ".local"
    emit (Instruction freshIdent "s" "truncd" [val])
    return ("%" <> freshIdent)
emitCast (Concrete "double" []) targetType val | isIntegerType targetType = do
    freshIdent <- newIdent ".local"
    emit (Instruction freshIdent (toQbeTy targetType) "dtosi" [val])
    return ("%" <> freshIdent)
emitCast (Concrete "float" []) targetType val | isIntegerType targetType = do
    freshIdent <- newIdent ".local"
    emit (Instruction freshIdent (toQbeTy targetType) "stosi" [val])
    return ("%" <> freshIdent)
emitCast parameterType targetType _ =
    fail ("Conversion from " ++ show parameterType ++ " to "
        ++ show targetType ++ " not implemented yet")

isFloatingType :: Type -> Bool
isFloatingType ty = doubleType == ty || floatType == ty

isIntegerType :: Type -> Bool
isIntegerType ty = charType == ty || shortType == ty || ushortType == ty
    || intType == ty || uintType == ty || longType == ty || ulongType == ty

literalToVal :: Literal -> Emit Val
literalToVal (StringLiteral str) = registerConstant str
literalToVal l = return (literalToText l)

literalToText :: Literal -> Text
literalToText (Int32 l) = pack (show l)
literalToText (UInt32 l) = pack (show l)
literalToText (Int64 l) = pack (show l)
literalToText (UInt64 l) = pack (show l)
literalToText (Float32 l) = "s_" <> pack (show l)
literalToText (Float64 l) = "d_" <> pack (show l)
literalToText (Bool True) = "1"
literalToText (Bool False) = "0"
literalToText (StringLiteral _) =
    error "String literals have to be handled with literalToVal"

pointerTy :: Ty
pointerTy = "l"

voidTy :: Ty
voidTy = ""

-- Handles sub-word types, used for data types and stores
-- sign does not matter
toQbeStoreTy :: Type -> Ty
toQbeStoreTy ty = case toQbeTy ty of
    "ub" -> "b"
    "sh" -> "h"
    "uh" -> "h"
    other -> other

-- Temporaries have to have at least word size
toQbeWordTy :: Type -> Ty
toQbeWordTy ty = case toQbeTy ty of
    "ub" -> "w"
    "sh" -> "w"
    "uh" -> "w"
    other -> other

-- Used for variables, smaller and unsigned types just fall back to w
toQbeTy :: Type -> Ty
toQbeTy (Concrete "bool" []) = "ub"
toQbeTy (Concrete "char" []) = "ub"
toQbeTy (Concrete "short" []) = "sh"
toQbeTy (Concrete "ushort" []) = "uh"
toQbeTy (Concrete "int" []) = "w"
-- loadw can be used insted of loaduw because signed does not matter
toQbeTy (Concrete "uint" []) = "w"
toQbeTy (Concrete "long" []) = "l"
toQbeTy (Concrete "ulong" []) = "l"
toQbeTy (Concrete "usize" []) = "l"
toQbeTy (Concrete "float" []) = "s"
toQbeTy (Concrete "double" []) = "d"
-- void type is just left empty for functions
toQbeTy (Concrete "void" []) = voidTy
toQbeTy (PointerType _) = pointerTy
-- TODO how to handle array types? always decay to pointer?
toQbeTy (ArrayType _ _) = pointerTy
toQbeTy (FunctionType _ _) = pointerTy
toQbeTy (Concrete "auto" []) = error "Error: auto appeared in printing stage"
toQbeTy (Concrete s []) = ":" <> s
toQbeTy other =
    error ("Error: generic or function type " ++ show other ++ " appeared in printing stage")

newLabel :: Text -> Emit Text
newLabel = newIdent

registerConstant :: Text -> Emit Text
registerConstant str = do
    freshName <- newIdent "string"
    ref <- asks getStringLiterals
    let escaped = "\"" <> escape str <> "\\0\""
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
statementToDef :: Statement -> Emit [Def]
statementToDef (Definition (Name name ty) (Literal l)) = do
    val <- literalToVal l
    return [DataDef name [(toQbeStoreTy ty, val)]]
statementToDef (FunctionDefintion name [] ty parameters statements) = do
    let qbeParams = fmap (\(Name n t) -> (toQbeTy t, n)) parameters
    let returnType = toQbeTy ty
    blocks <- withFreshBuilder (bodyToBlock (fmap snd qbeParams) statements)
    -- ensures that a block for a function ends with a ret
    let blocks' = addRetIfMissing blocks
    return [FuncDef returnType name qbeParams blocks']
statementToDef _ = return []

-- TODO solve problem with void functions
-- Following function:
-- void main() { putchar(100); }
-- would compile fine, but the ret would return the 100
-- as error code.
-- We can't simply end with ret 0, because this would be a syntax error for
-- void functions.
addRetIfMissing :: [Block] -> [Block]
addRetIfMissing blocks =
    if endsWithRet blocks
        then blocks
        else blocks ++ [Ret Nothing]

endsWithRet :: [Block] -> Bool
endsWithRet blocks =
    not (null blocks) && isRet (last blocks)

isRet :: Block -> Bool
isRet (Ret _) = True
isRet _ = False

isReturn :: Statement -> Bool
isReturn (Return _) = True
isReturn _ = False

structToDef :: Statement -> [Def]
structToDef (StructDefinition ident [] fields) =
    [TypeDef ident (fmap (\(Name _ ty) -> toQbeStoreTy ty) fields)]
structToDef _ = []

gatherGlobalNames :: [Statement] -> [Text]
gatherGlobalNames = foldMap gatherGlobalName

gatherGlobalName :: Statement -> [Text]
gatherGlobalName (Definition (Name name _) _) = [name]
gatherGlobalName (FunctionDefintion name _ _ _ _) = [name]
gatherGlobalName (ExternDefintion name _ _) = [name]
gatherGlobalName _ = []

prettyMod :: Mod -> Doc ann
prettyMod (Mod name defs) =
    fromText "# Compiled from" <+> fromText name <> ".quo"
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
