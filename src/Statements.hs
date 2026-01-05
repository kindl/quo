{-# LANGUAGE OverloadedStrings #-}
module Statements where

import Data.Functor(($>))
import Control.Applicative((<|>), optional, many, liftA3)
import Data.Attoparsec.Combinator(option)
import Types
import Expressions


moduleDefinition :: Parser Module
moduleDefinition = do
    m <- option (LocatedText "main" emptyLocation) (token "module" *> identifier <* token ";")
    s <- statements
    return (Module m s)

statements :: Parser [Statement]
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
    <|> breakStatement
    <|> continueStatement
    <|> whileStatement)

assignmentStatement :: Parser Statement
assignmentStatement =
    liftA2 Assignment statementExpression (token "=" *> expr) <* token ";"

callStatement :: Parser Statement
callStatement = do
    a@(Apply _ _) <- statementExpression <* token ";"
    return (Call a)

returnStatement :: Parser Statement
returnStatement = fmap Return (token "return" *> optional expr <* token ";")

importStatement :: Parser Statement
importStatement = liftA2 Import
    (token "import" *> identifier)
    (optional (curlies (sepByTrailing identifier (token ",")))) <* token ";"

externDefinition :: Parser Statement
externDefinition = do
    _ <- token "extern"
    t <- typeVariable
    i <- identifier
    params <- parens (sepByTrailing parameter (token ","))
    _ <- token ";"
    return (ExternDefintion i t params)

forStatement :: Parser Statement
forStatement = do
    _ <- token "for"
    (i, ty, e) <- parens forPart
    body <- curlies statements
    return (For (Name i ty) e body)

continueStatement :: Parser Statement
continueStatement = do
    _ <- token "continue"
    _ <- token ";"
    return ContinueStatement

breakStatement :: Parser Statement
breakStatement = do
    _ <- token "break"
    _ <- token ";"
    return BreakStatement

whileStatement :: Parser Statement
whileStatement = do
    _ <- token "while"
    e <- parens expr
    body <- curlies statements
    return (While e body)

forPart :: Parser (LocatedText, Type, Expression)
forPart =
    liftA3 (\ty i e -> (i, ty, e)) typeOrLet identifier (token "in" *> expr)

-- Consider turning switch into expression
-- The problem is, that this creates a circle:
-- Switch is an expression that contains statements
-- this circle exists for lambdas as well
-- Optional: only allow where switch can be easily turned into statements
-- for example:
-- let v = case { ... } can be turned into t v; case { ... -> v = expr }
-- return case { ... } can be turned into case { ... -> return expr }
switchStatement :: Parser Statement
switchStatement =
    liftA2 Switch (token "switch" *> parens expr) (curlies (many switchOptions))

switchOptions :: Parser (Expression, [Statement])
switchOptions =
    liftA2 (,) (token "case" *> expr) (token "=>" *> curlies statements)

definition :: Parser Statement
definition =
    liftA3 (\ty i e -> Definition (Name i ty) e) typeOrLet identifier (token "=" *> expr <* token ";")

typeOrLet :: Parser Type
typeOrLet = (token "let" $> auto) <|> typeOrAuto

typeOrAuto :: Parser Type
typeOrAuto = (token "auto" $> auto) <|> typeVariable

structDefinition :: Parser Statement
structDefinition = do
    i <- token "struct" *> identifier
    ts <- option [] typeNameParameters
    b <- curlies (many (parameter <* token ";"))
    _ <- token ";"
    return (StructDefinition i ts b)

functionDefintion :: Parser Statement
functionDefintion = do
    t <- typeVariable
    i <- identifier
    ts <- option [] typeNameParameters
    params <- parens (sepByTrailing parameter (token ","))
    body <- curlies statements
    return (FunctionDefintion i ts t params body)

parameter :: Parser Name
parameter = liftA2 (\ty i -> Name i ty) typeVariable identifier

ifStatement :: Parser Statement
ifStatement = do
    ifBranch <- ifPart
    elseIfBranches <- many (token "else" *> ifPart)
    elseBranch <- optional (token "else" *> curlies statements)
    return (If (ifBranch:elseIfBranches) elseBranch)

ifPart :: Parser (Expression, [Statement])
ifPart = token "if" *> liftA2 (,) (parens expr) (curlies statements)
