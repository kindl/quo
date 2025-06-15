{-# LANGUAGE OverloadedStrings #-}
module Qbe where

import Data.Text(Text, unpack, pack)
import Types
import Drucker
import Resolver(readType, literalType)
import Compiler(parens)
import Data.IORef
import Control.Monad.Trans.Reader(runReaderT, asks, ReaderT)
import Control.Monad.Trans.Class(lift)


-- Ident does not contain a sigil, while Val does
type Ident = Text
type Val = Text

type Ty = Text

data Mod = Mod Ident [Def]

data Block =
    Instruction Ident Ty [Val]
    | CallInstruction Ident Ty Val [(Ty, Val)]
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

toBlock :: [Statement] -> ReaderT Builder IO [()]
toBlock = traverse statementToBlock

statementToBlock :: Statement -> ReaderT Builder IO ()
statementToBlock (Return Nothing) =
    emit (Ret Nothing)
statementToBlock (Return (Just val)) = do
    returnVal <- expressionToVal' val
    emit (Ret (Just returnVal))
statementToBlock (Call expression) =
    expressionToVal expression >> return ()
-- TODO actual definition should not be a temporary but created on the stack
-- so basically
-- ptr <- alloc size of ty
-- emit code of expression
-- store in ptr
statementToBlock (Definition (Name name ty) (Apply expression expressions)) = do
    vals <- traverse expressionToVal expressions
    val <- expressionToVal' expression
    emit (CallInstruction ("%" <> name) (toQbeTy ty) val vals)
statementToBlock (While condition statements) = do
    val <- expressionToVal' condition
    whileLabel <- newLabel
    continueLabel <- newLabel
    --TODO recurse into statements
    emit (JumpNonZero val whileLabel continueLabel)
statementToBlock s = fail ("TODO " ++ show s)

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

toQbeTy :: Type -> Text
toQbeTy (Concrete "char" []) = "b"
toQbeTy (Concrete "int" []) = "w"
toQbeTy (Concrete "long" []) = "l"
toQbeTy (Concrete "auto" []) = error "Error: auto appeared in printing stage"
toQbeTy (Concrete s []) = ":" <> s
toQbeTy (Concrete s typeParameters) =
    error ("Error: generic type "
        ++ unpack s ++ show typeParameters ++ " appeared in printing stage")

newLabel :: ReaderT Builder IO Text
newLabel = newIdent "label"

registerConstant :: Text -> ReaderT Builder IO Text
registerConstant str = do
    freshName <- newIdent "string"
    ref <- asks (\(Builder _ constants _) -> constants)
    let escaped = "\"" <> escape str <> "\""
    lift (modifyIORef' ref (\c -> c ++ [DataDef freshName [("b", escaped), ("b", "0")]]))
    return ("$" <> freshName)


-- Pretty Printing to Qbe format
moduleToDefs :: Module -> IO Mod
moduleToDefs (Module name statements) = do
    defs <- fmap concat (traverse statementToDef statements)
    return (Mod name defs)

-- Top level
statementToDef :: Statement -> IO [Def]
statementToDef (Definition (Name name ty) (Literal l)) =
    let
        val = literalToText l
    in return [DataDef name [(toQbeTy ty, val)]]
statementToDef (FunctionDefintion name [] ty parameters statements) = do
    numberSupply <- newIORef 0
    globals <- newIORef []
    instructions <- newIORef []
    _ <- runReaderT (toBlock statements) (Builder numberSupply globals instructions)
    blocks <- readIORef instructions
    globalDefs <- readIORef globals
    return (globalDefs ++ [FuncDef (toQbeTy ty) name (fmap (\(Name n t) -> (toQbeTy t, n)) parameters) blocks])
statementToDef _ = return []

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
    "type" <+> ":" <> fromText ident <+> "=" <+> "{"
        <> intercalate ", " (fmap prettyData fields)
        <> "}"
prettyDef (TypeDef ident fields) =
    "type" <+> ":" <> fromText ident <+> "=" <+> "{"
        <> intercalate ", " (fmap prettyTy fields)
        <> "}"

prettyBlock :: Block -> Document
prettyBlock (Instruction ident ty parameters) =
    indent ("%" <> fromText ident <+> "=" <> prettyTy ty <+> (intercalate ", " (fmap prettyVal parameters)))
prettyBlock (CallInstruction ident ty f parameters) =
    indent ("%" <> fromText ident <+> "=" <> prettyTy ty <+> "call" <+> fromText f <> parens (intercalate ", " (fmap prettyParam parameters)))
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
