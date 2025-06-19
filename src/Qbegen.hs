{-# LANGUAGE OverloadedStrings #-}
module Qbegen(moduleToQbe, prettyMod) where

import Data.Text(Text, pack)
import Types
import Resolver(literalType)
import Data.IORef(IORef, modifyIORef', newIORef, readIORef)
import Control.Monad(when)
import Control.Monad.Trans.Reader(ReaderT, runReaderT, asks, local)
import Control.Monad.Trans.Class(lift)
import Data.Foldable(traverse_)
import Data.Int(Int32)
import Prettyprinter((<+>), Doc, parens)
import Helpers((<//>), indent, fromText, intercalate, escape)


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

data Builder = Builder
    -- List of global names for determining sigil
    [Text]
    -- List of parameters
    [Text]
    -- Name supply
    (IORef Int)
    -- Global data for string literals
    (IORef [Def])
    -- Instructions
    (IORef [Block])


newUnique :: ReaderT Builder IO Int
newUnique = do
    ref <- asks (\(Builder _ _ supply _ _) -> supply)
    lift (modifyIORef' ref succ >> readIORef ref)

newIdent :: Text -> ReaderT Builder IO Text
newIdent prefix = fmap (\i -> prefix <> pack (show i)) newUnique

emit :: Block -> ReaderT Builder IO ()
emit instruction = do
    ref <- asks (\(Builder _ _ _ _ instructions) -> instructions)
    lift (modifyIORef' ref (\instructions -> instructions ++ [instruction]))

withParameters :: [Text] -> ReaderT Builder m a -> ReaderT Builder m a
withParameters params action =
    local (\(Builder globals _ uniques stringDefs instructions) ->
        Builder globals params uniques stringDefs instructions) action

withFreshBuilder :: ReaderT Builder IO () -> ReaderT Builder IO [Block]
withFreshBuilder action = do
    instructions <- lift (newIORef [])
    local (\(Builder params globals uniques stringDefs _) -> Builder params globals uniques stringDefs instructions) action
    lift (readIORef instructions)

isGlobal :: Text -> ReaderT Builder IO Bool
isGlobal name = do
    globalNames <- asks (\(Builder g _ _ _ _) -> g)
    return (elem name globalNames)
    
isParam :: Text -> ReaderT Builder IO Bool
isParam name = do
    params <- asks (\(Builder _ p _ _ _) -> p)
    return (elem name params)

toBlock :: [Statement] -> ReaderT Builder IO ()
toBlock = traverse_ statementToBlock

bodyToBlock :: [Text] -> [Statement] -> ReaderT Builder IO ()
bodyToBlock parameters statements =
    let
        locals = getDefs statements
    in withParameters parameters
        (traverse_ emitAlloc locals >> toBlock statements)

emitAlloc :: Name -> ReaderT Builder IO ()
emitAlloc (Name name ty) =
    emit (Instruction name pointerTy ("alloc" <> getAlignmentAsText ty) [getSizeAsVal ty])


getDefs :: [Statement] -> [Name]
getDefs = foldMap getDef

getDef :: Statement -> [Name]
getDef (Definition name _) = [name]
getDef (If branches maybeElseBranch) =
    foldMap (getDefs . snd) branches ++ getDefs (concat maybeElseBranch)
getDef (While _ statements) =
    getDefs statements
getDef (For _ _ statements) =
    getDefs statements
getDef _ = []

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
statementToBlock (Definition (Name name ty) expression) = do
    -- The allocation for locals are not done here, but per function.
    -- This is necessary so that we do not keep reallocating,
    -- for example in a while-loop
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
statementToBlock s = fail ("TODO " ++ show s)

-- TODO calculate sizes
-- we either need a struct environment or annotate sizes in a previous pass
getSizeAsVal :: Type -> Text
getSizeAsVal ty = pack (show (getSize ty))

getAlignmentAsText :: Type -> Text
getAlignmentAsText ty = pack (show (getAlignment ty))

getAlignment :: Type -> Int32
getAlignment (PointerType _) = 8
getAlignment (Concrete "int" []) = 4
-- TODO
getAlignment _ = 4

pointerSize :: Int32
pointerSize = 8

getSize :: Type -> Int32
getSize (Concrete "char" []) = 1
getSize (Concrete "int" []) = 4
getSize (Concrete "long" []) = 8
getSize (Concrete "float" []) = 4
getSize (Concrete "double" []) = 8
getSize (PointerType _) = pointerSize
getSize (ArrayType ty (Just size)) = getSize ty * size
--getSize (ArrayType ty Nothing) = pointerSize
getSize other = error ("Cannot determine size of type " ++ show other)

expressionToVal' :: Expression -> ReaderT Builder IO Ident
expressionToVal' e = do
    (_, v) <- expressionToVal e
    return v

expressionToVal :: Expression -> ReaderT Builder IO (Ty, Val)
expressionToVal (Variable (Name name ty) []) = do
    let qbeTy = toQbeTy ty
    wasParam <- isParam name
    if wasParam
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
expressionToVal (Apply (Variable (Name name (FunctionType returnType _)) []) expressions) = do
    -- Turn a nested call like f(g())
    -- into
    -- %a = g()
    -- f(%a)
    freshIdent <- newIdent ".local"
    vals <- traverse expressionToVal expressions
    let qbeTy = toQbeTy returnType
    if isOperator name
        then emitOperator freshIdent qbeTy name vals
        -- TODO Investigate: Does qbe allow calling local functions?
        else emit (CallInstruction freshIdent qbeTy ("$" <> name) vals)
    return (qbeTy, "%" <> freshIdent)
expressionToVal (Apply expression expressions) = do
    freshIdent <- newIdent ".local"
    vals <- traverse expressionToVal expressions
    (returnType, val) <- expressionToVal expression
    emit (CallInstruction freshIdent returnType val vals)
    return (returnType, "%" <> freshIdent)

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
    ref <- asks (\(Builder _ _ _ stringDefs _) -> stringDefs)
    let escaped = "\"" <> escape str <> "\\000\""
    lift (modifyIORef' ref (\c -> c ++ [DataDef freshName [("b", escaped)]]))
    return ("$" <> freshName)


-- Pretty Printing to Qbe format
moduleToQbe :: Module -> IO Mod
moduleToQbe (Module name statements) = do
    uniques <- newIORef 0
    stringDefs <- newIORef []
    instructions <- newIORef []
    let globalNames = getGlobalNames statements
    let baseBuilder = Builder globalNames [] uniques stringDefs instructions
    defs <- runReaderT (traverse statementToDef statements) baseBuilder
    createdStringDefs <- readIORef stringDefs
    return (Mod name (createdStringDefs ++ concat defs))

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

getGlobalNames :: [Statement] -> [Text]
getGlobalNames = foldMap getGlobalName

getGlobalName :: Statement -> [Text]
getGlobalName (Definition (Name name _) _) = [name]
getGlobalName (FunctionDefintion name _ _ _ _) = [name]
getGlobalName _ = []

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
