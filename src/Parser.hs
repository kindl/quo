{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Applicative
import Data.List(uncons)
import Data.Either(lefts, rights)
import Data.Functor(($>))
import Data.Attoparsec.Combinator(sepBy', eitherP, option)
import Control.Monad.Trans.State(StateT(..), runStateT)
import Types
import qualified Data.Text.IO as Text
import Lexer(lexe)


-- TODO merge common parts in Rslike and Clike
-- They are mostly the same and only the statements are different
parseFile parser fileName = fmap (parse parser) (Text.readFile fileName)

lexeFile fileName = fmap lexe (Text.readFile fileName)

parse parser str = case lexe str of
    Left e -> Left e
    Right l -> case runStateT parser l of
        Just (block, []) -> Right block
        other -> Left (show other)

next = StateT uncons

-- This is for definitions, like functions
typeNameParameters = angles (sepBy' identifier (token ","))

typeParameters = angles (sepBy' typ (token ","))

typ = (token "auto" $> auto) <|> typeVariable

typeVariable = do
    t <- identifier
    ts <- option [] typeParameters
    return (TypeVariable t ts)

optionalType = option auto typ

-- operators
expr = ternaryop

-- a = wasTrue? thenCase : elseCase
ternaryop = liftA3 makeTernaryOp orop (token "?" *> optional expr) (token ":" *> expr) <|> orop

makeTernaryOp a thenBranch b =
    Apply auto (Variable Local "?:" []) ([a] ++ maybe [] return thenBranch ++ [b])

-- logical operators
orop = leftassoc makeBinaryOp (token "||") andop
andop = leftassoc makeBinaryOp (token "&&") compareop
compareop = leftassoc makeBinaryOp (token "<" <|> token ">" <|> token "<=" <|> token ">=" <|> token "!=" <|> token "==") addop

-- arithmethic operators
addop = leftassoc makeBinaryOp (token "+" <|> token "-") mulop
mulop = leftassoc makeBinaryOp (token "*" <|> token "/" <|> token "%") expop
expop = liftA3 makeBinaryOp unop (token "^") expop <|> unop

makeBinaryOp a opName b = Apply auto (Variable Local opName []) [a, b]

leftassoc g op p = liftA2 (foldl (flip id)) p (many (liftA2 (\o a b -> g b o a) op p))

-- unary operators
unop = liftA2 makeUnaryOp (token "!" <|> token "-") prefixexpr <|> prefixexpr

makeUnaryOp opName a = Apply auto (Variable Local opName []) [a]

-- TODO
templateString = do
    TemplateStringBegin <- next
    stringsAndExpressions <- many (eitherP templateStringMid expr)
    TemplateStringEnd <- next
    let parameter1 = ArrayExpression (lefts stringsAndExpressions)
    let parameter2 = ArrayExpression (rights stringsAndExpressions)
    return (Apply auto (Variable Local "format" []) [parameter1, parameter2])

templateStringMid = do
    TemplateStringMid s <- next
    return (String s)


literal = integer <|> double <|> string <|> bool

arrayExpression = fmap ArrayExpression (squares (sepBy' expr (token ",")))

-- TODO decide if we should be able to parse 2.ToString()

-- Trick to parse left recursive
prefixexpr = liftA2 (foldl (\e f -> f e))
    (literal <|> variable <|> parens expr <|> arrayExpression)
    (many (squareAccess <|> dotAcces <|> parameterList))

-- a.b
dotAcces = liftA2 (\i ts e -> DotAccess e i ts)
    (token "." *> identifier)
    (option [] typeParameters)

-- a[2]
-- a[2 + a]
squareAccess = fmap (flip SquareAccess) (squares expr)

-- a(2, 3)
-- a<int>(2, 3)
parameterList = fmap (flip (Apply auto)) (parens (sepBy' expr (token ",")))

variable = liftA2 (Variable Local) identifier (option [] typeParameters)

-- helper functions to transform tokens to values
integer = do
    Integer n <- next
    return (Int64 n)

double = do
    Double n <- next
    return (Float64 n)

string = do
    NonTemplateString s <- next
    return (String s)

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
