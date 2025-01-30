{-# LANGUAGE OverloadedStrings #-}
module Clike where

import Control.Applicative((<|>), optional, many, liftA2, liftA3)
import Data.Attoparsec.Combinator(option)
import Types
import Parser


statements = many (functionDefintion
    <|> externDefinition
    <|> importStatement
    <|> callStatement
    <|> ifStatement
    <|> switchStatement
    <|> definition
    <|> structDefinition
    <|> returnStatement
    <|> forStatement
    <|> whileStatement)

callStatement = do
    a@(Apply _ _ _) <- statementExpression <* token ";"
    return (Call a)

returnStatement = fmap Return (token "return" *> optional expr <* token ";")

importStatement = liftA2 Import
    (token "import" *> identifier)
    (optional (curlies (sepByTrailing identifier (token ",")))) <* token ";"

externDefinition = do
    _ <- token "extern"
    t <- typ
    name <- identifier
    params <- parens (sepByTrailing parameter (token ","))
    _ <- token ";"
    return (ExternDefintion name t params)

forStatement = do
    _ <- token "for"
    (i, t, e) <- parens forPart
    body <- curlies statements
    return (For i t e body)

whileStatement = do
    _ <- token "while"
    e <- parens expr
    body <- curlies statements
    return (While e body)

forPart =
    liftA3 (\t i e -> (i, t, e)) typ identifier (token "in" *> expr)

-- Consider turning switch into expression
-- The problem is, that this creates a circle:
-- Switch is an expression that contains statements
-- this circle exists for lambdas as well
switchStatement =
    liftA2 Switch (token "switch" *> parens expr) (curlies (many switchOptions))

switchOptions =
    liftA2 (,) (token "case" *> expr) (token "=>" *> curlies statements)

definition =
    liftA3 (flip Definition) typeOrLet identifier (token "=" *> expr <* token ";")

typeOrLet = (token "let" *> return auto) <|> typ

structDefinition = do
    i <- token "struct" *> identifier
    ts <- option [] typeNameParameters
    b <- curlies (many (parameter <* token ";"))
    _ <- token ";"
    return (StructDefinition i ts b)

functionDefintion = do
    t <- typ
    name <- identifier
    ts <- option [] typeNameParameters
    params <- parens (sepByTrailing parameter (token ","))
    body <- curlies statements
    return (FunctionDefintion name t ts params body)

parameter = liftA2 (flip (,)) typ identifier

ifStatement = do
    ifBranch <- ifPart
    elseIfBranches <- many (token "else" *> ifPart)
    elseBranch <- optional (token "else" *> curlies statements)
    return (If (ifBranch:elseIfBranches) elseBranch)

ifPart = token "if" *> liftA2 (,) (parens expr) (curlies statements)
