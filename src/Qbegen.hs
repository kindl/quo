{-# LANGUAGE OverloadedStrings #-}
module Qbegen where

import Data.Text(Text, pack)
import Types
import Resolver(readType, literalType)
import Cgen(parens)
import Data.IORef
import Control.Monad.Trans.Reader(runReaderT, asks, ReaderT, local)
import Control.Monad.Trans.Class(lift)
import Data.Foldable(traverse_)
import Data.Int(Int32)
import Prettyprinter((<+>), Doc)
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
    -- Name supply
    (IORef Int)
    -- Global data for string literals
    (IORef [Def])
    -- Instructions
    (IORef [Block])


newUnique :: ReaderT Builder IO Int
newUnique = do
    ref <- asks (\(Builder _ supply _ _) -> supply)
    lift (modifyIORef' ref succ >> readIORef ref)

newIdent :: Text -> ReaderT Builder IO Text
newIdent prefix = fmap (\i -> prefix <> pack (show i)) newUnique

emit :: Block -> ReaderT Builder IO ()
emit instruction = do
    ref <- asks (\(Builder _ _ _ instructions) -> instructions)
    lift (modifyIORef' ref (\instructions -> instructions ++ [instruction]))

withFreshBuilder action = do
    instructions <- lift (newIORef [])
    local (\(Builder globals uniques stringDefs _) -> Builder globals uniques stringDefs instructions) action
    lift (readIORef instructions)

isGlobal :: Text -> ReaderT Builder IO Bool
isGlobal name = do
    globalNames <- asks (\(Builder g _ _ _) -> g)
    return (elem name globalNames)

toBlock :: [Statement] -> ReaderT Builder IO ()
toBlock = traverse_ statementToBlock

statementToBlock :: Statement -> ReaderT Builder IO ()
statementToBlock (Return Nothing) =
    emit (Ret Nothing)
statementToBlock (Return (Just val)) = do
    returnVal <- expressionToVal' val
    emit (Ret (Just returnVal))
statementToBlock (Call expression) =
    expressionToVal expression >> return ()
statementToBlock (Assignment leftHand rightHand) = do
    -- TODO ensure that leftHand produces a memory reference
    addressVal <- expressionToVal' leftHand
    (ty, val) <- expressionToVal rightHand
    emit (Store ty val addressVal)
-- TODO actual definition should not be a temporary but created on the stack
-- so basically
-- ptr <- alloc size of ty
-- emit code of expression
-- store in ptr
statementToBlock (Definition (Name name ty) expression) = do
    emit (Instruction name pointerTy ("alloc" <> getAlignmentAsText ty) [getSizeAsVal ty])
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
getSizeAsVal ty = pack (show (getSize ty))

getAlignmentAsText ty = pack (show (getAlignment ty))

getAlignment (PointerType _) = 8
getAlignment (Concrete "int" []) = 4
-- TODO
getAlignment _ = 4

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
    freshIdent <- newIdent ".local"
    isGlobalVar <- isGlobal name
    let sigil = if isGlobalVar then "$" else "%"
    let qbeTy = toQbeTy ty
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
    -- TODO Investigate: Does qbe allow calling local functions?
    emit (CallInstruction freshIdent qbeTy ("$" <> name) vals)
    return (qbeTy, "%" <> freshIdent)
expressionToVal (Apply expression expressions) = do
    freshIdent <- newIdent ".local"
    vals <- traverse expressionToVal expressions
    (returnType, val) <- expressionToVal expression
    emit (CallInstruction freshIdent returnType val vals)
    return (returnType, "%" <> freshIdent)

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

pointerTy = "l"

voidTy = ""

toQbeTy :: Type -> Text
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
    ref <- asks (\(Builder _ _ stringDefs _) -> stringDefs)
    let escaped = "\"" <> escape str <> "\\000\""
    lift (modifyIORef' ref (\c -> c ++ [DataDef freshName [("b", escaped)]]))
    return ("$" <> freshName)


-- Pretty Printing to Qbe format
moduleToQbe (Module name statements) = do
    uniques <- newIORef 0
    stringDefs <- newIORef []
    instructions <- newIORef []
    let globalNames = getGlobalNames statements
    defs <- runReaderT (traverse statementToDef statements) (Builder globalNames uniques stringDefs instructions)
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
    blocks <- withFreshBuilder (toBlock statements)
    return [FuncDef (toQbeTy ty) name (fmap (\(Name n t) -> (toQbeTy t, n)) parameters) blocks]
statementToDef _ = return []

getGlobalNames = foldMap getGlobalName

getGlobalName (Definition (Name name _) _) = [name]
getGlobalName (FunctionDefintion name _ _ _ _) = [name]
getGlobalName _ = []

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


prettyTy :: Text -> Doc a
prettyTy ty = fromText ty

prettyVal :: Text -> Doc a
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
    indent ("%" <> fromText ident <+> "=" <> prettyTy ty <+> fromText instr <+> (intercalate ", " (fmap prettyVal parameters)))
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
