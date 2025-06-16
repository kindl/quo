{-# LANGUAGE OverloadedStrings #-}
module Qbegen where

import Data.Text(Text, unpack, pack)
import Types
import Drucker
import Resolver(readType, literalType)
import Cgen(parens)
import Data.IORef
import Control.Monad.Trans.Reader(runReaderT, asks, ReaderT, local)
import Control.Monad.Trans.Class(lift)
import Data.Foldable(traverse_)
import Data.Int(Int32)


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
    -- Name supply
    (IORef Int)
    -- Global data for string literals
    (IORef [Def])
    -- Instructions
    (IORef [Block])


newUnique :: ReaderT Builder IO Int
newUnique = do
    ref <- asks (\(Builder supply _ _) -> supply)
    lift (modifyIORef' ref succ >> readIORef ref)

newIdent :: Text -> ReaderT Builder IO Text
newIdent prefix = fmap (\i -> prefix <> pack (show i)) newUnique

emit :: Block -> ReaderT Builder IO ()
emit instruction = do
    ref <- asks (\(Builder _ _ instructions) -> instructions)
    lift (modifyIORef' ref (\instructions -> instructions ++ [instruction]))

withFreshBuilder action = do
    instructions <- lift (newIORef [])
    local (\(Builder uniques globals _) -> Builder uniques globals instructions) action
    lift (readIORef instructions)
    

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
    emit (Instruction name pointerTy "alloca4" [getSizeAsVal ty])
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
    -- TODO referring to globals
    return (toQbeTy ty, "%" <> name)
expressionToVal (Literal l) = do
    val <- literalToVal l
    return (toQbeTy (literalType l), val)
-- Turn a nested call like f(g())
-- into
-- %a = g()
-- f(%a)
expressionToVal (Apply (Variable (Name name (FunctionType returnType _)) []) expressions) = do
    freshIdent <- newIdent "local"
    vals <- traverse expressionToVal expressions
    let ty = toQbeTy returnType
    emit (CallInstruction freshIdent ty ("%" <> name) vals)
    return (ty, "%" <> freshIdent)
expressionToVal (Apply expression expressions) = do
    freshIdent <- newIdent "local"
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
    ref <- asks (\(Builder _ constants _) -> constants)
    let escaped = "\"" <> escape str <> "\""
    lift (modifyIORef' ref (\c -> c ++ [DataDef freshName [("b", escaped), ("b", "0")]]))
    return ("$" <> freshName)


-- Pretty Printing to Qbe format
moduleToQbe (Module name statements) = do
    uniques <- newIORef 0
    globals <- newIORef []
    instructions <- newIORef []
    defs <- runReaderT (traverse statementToDef statements) (Builder uniques globals instructions)
    return (Mod name (concat defs))

-- Top level
statementToDef :: Statement -> ReaderT Builder IO [Def]
statementToDef (Definition (Name name ty) (Literal l)) =
    let
        val = literalToText l
    in return [DataDef name [(toQbeTy ty, val)]]
-- TODO ensure that a block for a function ends with a ret
-- we can't just check if the last block stat is a ret
statementToDef (FunctionDefintion name [] ty parameters statements) = do
    blocks <- withFreshBuilder (toBlock statements)
    return [FuncDef (toQbeTy ty) name (fmap (\(Name n t) -> (toQbeTy t, n)) parameters) blocks]
statementToDef _ = return []


prettyMod (Mod name defs) =
    fromText "# Compiled from" <+> fromText name
    <//> intercalate "\n\n" (fmap prettyDef defs)

-- For definitions
prettyFunParam :: (Ty, Ident) -> Document
prettyFunParam (ty, ident) = prettyTy ty <+> "%" <> fromText ident

-- For calls
prettyParam :: (Ty, Val) -> Document
prettyParam (ty, val) = prettyTy ty <+> prettyVal val

-- For global data
prettyData :: (Ty, Val) -> Document
prettyData = prettyParam


prettyTy :: Text -> Document
prettyTy ty = fromText ty

prettyVal :: Text -> Document
prettyVal val = fromText val

prettyDef :: Def -> Document
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

prettyBlock :: Block -> Document
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
