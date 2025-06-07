{-# LANGUAGE OverloadedStrings #-}
module Statements where

import Data.Functor(($>))
import Control.Applicative((<|>), optional, many, liftA3)
import Data.Attoparsec.Combinator(option)
import Types
import Expressions


moduleDefinition = do
    m <- option "main" (token "module" *> identifier <* token ";")
    s <- statements
    return (Module m s)

statements = many (functionDefintion
    <|> externDefinition
    <|> importStatement
    <|> assignmentStatement
    <|> callStatement
    <|> ifStatement
    <|> switchStatement
    <|> definition
    <|> structDefinition
    <|> returnStatement
    <|> forStatement
    <|> whileStatement)

assignmentStatement =
    liftA2 Assignment statementExpression (token "=" *> expr) <* token ";"

callStatement = do
    a@(Apply _ _) <- statementExpression <* token ";"
    return (Call a)

returnStatement = fmap Return (token "return" *> optional expr <* token ";")

importStatement = liftA2 Import
    (token "import" *> identifier)
    (optional (curlies (sepByTrailing identifier (token ",")))) <* token ";"

externDefinition = do
    _ <- token "extern"
    t <- typeVariable
    i <- identifier
    params <- parens (sepByTrailing parameter (token ","))
    _ <- token ";"
    return (ExternDefintion i t params)

forStatement = do
    _ <- token "for"
    (i, t, e) <- parens forPart
    body <- curlies statements
    return (For (Name i t) e body)

whileStatement = do
    _ <- token "while"
    e <- parens expr
    body <- curlies statements
    return (While e body)

forPart =
    liftA3 (\t i e -> (i, t, e)) typeOrLet identifier (token "in" *> expr)

-- Consider turning switch into expression
-- The problem is, that this creates a circle:
-- Switch is an expression that contains statements
-- this circle exists for lambdas as well
-- Optional: only allow where switch can be easily turned into statements
-- for example:
-- let v = case { ... } can be turned into t v; case { ... -> v = expr }
-- return case { ... } can be turned into case { ... -> return expr }
switchStatement =
    liftA2 Switch (token "switch" *> parens expr) (curlies (many switchOptions))

switchOptions =
    liftA2 (,) (token "case" *> expr) (token "=>" *> curlies statements)

definition =
    liftA3 (\t i e -> Definition (Name i t) e) typeOrLet identifier (token "=" *> expr <* token ";")

typeOrLet = (token "let" $> auto) <|> typeOrAuto

typeOrAuto = (token "auto" $> auto) <|> typeVariable

structDefinition = do
    i <- token "struct" *> identifier
    ts <- option [] typeNameParameters
    b <- curlies (many (parameter <* token ";"))
    _ <- token ";"
    return (StructDefinition i ts b)

functionDefintion = do
    t <- typeVariable
    i <- identifier
    ts <- option [] typeNameParameters
    params <- parens (sepByTrailing parameter (token ","))
    body <- curlies statements
    return (FunctionDefintion i ts t params body)

parameter = liftA2 (flip Name) typeVariable identifier

ifStatement = do
    ifBranch <- ifPart
    elseIfBranches <- many (token "else" *> ifPart)
    elseBranch <- optional (token "else" *> curlies statements)
    return (If (ifBranch:elseIfBranches) elseBranch)

ifPart = token "if" *> liftA2 (,) (parens expr) (curlies statements)
