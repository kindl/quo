{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Applicative
import Data.List(uncons)
import Data.Either(lefts, rights)
import Data.Functor(($>))
import Data.Attoparsec.Combinator(eitherP, option)
import Control.Monad.Trans.State.Strict(StateT(..), runStateT)
import Types
import qualified Data.Text.IO as Text
import Lexer(lexe)


-- TODO merge common parts in Golike and Clike
-- They are mostly the same and only the statements are different
parseFile parser fileName = fmap (parse parser) (Text.readFile fileName)

lexeFile fileName = fmap lexe (Text.readFile fileName)

parse parser str = case lexe str of
    Left e -> Left e
    Right l -> case runStateT parser l of
        Just (block, []) -> Right block
        other -> Left (show other)

next = StateT uncons

-- This is for definitions, like functions and structs
-- `struct Example<T> { }`
typeNameParameters = angles (sepByTrailing identifier (token ","))

typeParameters = angles (sepByTrailing typ (token ","))

typ = (token "auto" $> auto) <|> typeVariable

typeVariable = do
    t <- identifier
    ts <- option [] typeParameters
    arraySize <- optional (squares integer)
    return (TypeVariable t ts arraySize)

-- operators
expr = conditionalOp

-- a = wasTrue? thenCase : elseCase
conditionalOp =
    liftA3 makeConditionalOp orOp (token "?" *> optional expr) (token ":" *> expr) <|> orOp

makeConditionalOp a thenBranch b =
    Apply auto (Variable "?:" []) ([a] ++ maybe [] return thenBranch ++ [b])

-- logical operators
orOp = leftAssoc makeBinaryOp (token "||") andOp
andOp = leftAssoc makeBinaryOp (token "&&") compareOp
compareOp = leftAssoc makeBinaryOp (token "<" <|> token ">" <|> token "<=" <|> token ">=" <|> token "!=" <|> token "==") addOp

-- arithmethic operators
addOp = leftAssoc makeBinaryOp (token "+" <|> token "-") mulOp
mulOp = leftAssoc makeBinaryOp (token "*" <|> token "/" <|> token "%") expoOp
expoOp = liftA3 makeBinaryOp unOp (token "^") expoOp <|> unOp

makeBinaryOp a opName b = Apply auto (Variable opName []) [a, b]

leftAssoc g op p = liftA2 (foldl (flip id)) p (many (liftA2 (\o a b -> g b o a) op p))

-- unary operators
unOp = liftA2 makeUnaryOp (token "!" <|> token "-") postfixExpression <|> postfixExpression

makeUnaryOp opName a = Apply auto (Variable opName []) [a]

-- TODO
templateString = do
    TemplateStringBegin <- next
    stringsAndExpressions <- many (eitherP templateStringMid expr)
    TemplateStringEnd <- next
    let parameter1 = ArrayExpression (lefts stringsAndExpressions)
    let parameter2 = ArrayExpression (rights stringsAndExpressions)
    return (Apply auto (Variable "format" []) [parameter1, parameter2])

templateStringMid = do
    TemplateStringMid s <- next
    return (String s)


literal = fmap Int64 integer <|> fmap Float64 double <|> fmap String string <|> bool

arrayExpression = fmap ArrayExpression (squares (sepByTrailing expr (token ",")))

-- TODO decide if we should be able to parse 2.ToString()

-- Trick to parse left recursive
postfixExpression = liftA2 (foldl (\e f -> f e))
    primaryExpression
    (many (squareAccess <|> dotAcces <|> parameterList))

-- Could be something like
-- `liftA2 (\e f -> f e) postfixExpression parameterList`
-- but postfixExpression consumes everything
statementExpression = postfixExpression

primaryExpression =
    literal <|> variable <|> parens expr <|> arrayExpression

-- a.b
dotAcces = liftA2 (\i ts e -> DotAccess e i ts)
    (token "." *> identifier)
    (option [] typeParameters)

-- a[2]
-- a[2 + a]
squareAccess = fmap (flip SquareAccess) (squares expr)

-- a(2, 3)
-- a<int>(2, 3)
parameterList = fmap (flip (Apply auto)) (parens (sepByTrailing expr (token ",")))

variable = liftA2 Variable identifier (option [] typeParameters)

-- helper functions to transform tokens to values
integer = do
    Integer n <- next
    return n

double = do
    Double n <- next
    return n

string = do
    NonTemplateString s <- next
    return s

bool = (token "true" $> Boolean True) <|> (token "false" $> Boolean False)

identifier = do
    Identifier i <- next
    return i

token t = do
    Special s <- next
    if s == t then return t else fail "token"

parens p = token "(" *> p <* token ")"
squares p = token "[" *> p <* token "]"
curlies p = token "{" *> p <* token "}"
angles p = token "<" *> p <* token ">"

sepBy1Trailing p sep =
    liftA2 (:) p (many (sep *> p)) <* optional sep

sepByTrailing p sep = sepBy1Trailing p sep <|> return []