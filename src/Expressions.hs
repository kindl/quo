{-# LANGUAGE OverloadedStrings #-}
module Expressions where

import Control.Applicative
import Data.List(uncons)
import Data.Either(lefts, rights)
import Data.Functor(($>))
import Data.Attoparsec.Combinator(eitherP, option, sepBy)
import Control.Monad.Trans.State.Strict(StateT(..), runStateT)
import Types
import Lexer(tokenize, Token(..))
import qualified Data.Text.IO as Text
import Data.Text(Text, pack)
import Data.Int(Int32, Int64)
import Data.Word(Word32, Word64)


type Parser a = StateT [Token] Maybe a

parseFile :: String -> Parser b -> IO b
parseFile file parser = do
    contents <- Text.readFile file
    either fail return (parse file parser contents)

tokenizeFile :: FilePath -> IO (Either String [Token])
tokenizeFile file =
    fmap (tokenize (pack file)) (Text.readFile file)

parse :: String -> Parser a -> Text -> Either String a
parse file parser str = case tokenize (pack file) str of
    Left err -> Left err
    Right ts -> case runStateT parser ts of
        Just (result, []) -> Right result
        Just (_, rest) ->
            Left ("Could not parse til end " ++ show (take 3 rest))
        Nothing -> Left ("Could not tokenize file " ++ file)

next :: Parser Token
next = StateT uncons

-- This is for definitions, like functions and structs
-- `struct Example<T> { }`
typeNameParameters :: Parser [Text]
typeNameParameters = angles (sepByTrailing identifier (token ","))

typeParameters :: Parser [Type]
typeParameters = angles (sepByTrailing typeVariable (token ","))

typeVariable :: Parser Type
typeVariable = do
    t <- identifier
    ts <- option [] typeParameters
    arraySizes <- many (squares (optional integer))
    return (foldl ArrayType (makeConcrete t ts) arraySizes)

-- operators
expr :: Parser Expression
expr = conditionalOp

-- a = wasTrue? thenCase : elseCase
conditionalOp :: Parser Expression
conditionalOp =
    (makeConditionalOp <$> orOp <*> fmap snd (operator "?") <*> optional expr <*> (operator ":" *> expr))
    <|> orOp

makeConditionalOp :: Expression -> Location -> Maybe Expression -> Expression -> Expression
makeConditionalOp a loc thenBranch b =
    Apply (Variable (Name "?:" auto loc) []) ([a] ++ maybe [] return thenBranch ++ [b])

-- logical operators
orOp :: Parser Expression
orOp = leftAssoc makeBinaryOp (operator "||") andOp

andOp :: Parser Expression
andOp = leftAssoc makeBinaryOp (operator "&&") compareOp

compareOp :: Parser Expression
compareOp =
    leftAssoc makeBinaryOp (operator "<" <|> operator ">" <|> operator "<=" <|> operator ">=" <|> operator "!=" <|> operator "==") addOp

-- arithmethic operators
addOp :: Parser Expression
addOp = leftAssoc makeBinaryOp (operator "+" <|> operator "-") mulOp

mulOp :: Parser Expression
mulOp = leftAssoc makeBinaryOp (operator "*" <|> operator "/" <|> operator "%") expoOp

expoOp :: Parser Expression
expoOp = liftA3 makeBinaryOp unOp (operator "^") expoOp <|> unOp

makeBinaryOp :: Expression -> (Text, Location) -> Expression -> Expression
makeBinaryOp a (opName, loc) b =
    Apply (Variable (Name opName auto loc) []) [a, b]

leftAssoc :: Alternative f => (t -> sep -> t -> t) -> f sep -> f t -> f t
leftAssoc g op p = liftA2 (foldl (flip id)) p (many (liftA2 (\o a b -> g b o a) op p))

-- unary operators
unOp :: Parser Expression
unOp = liftA2 makeUnaryOp (operator "!" <|> unaryMinus) postfixExpression
    <|> postfixExpression

-- unary minus uses "-_" to differentiate between negation and subtraction
unaryMinus :: Parser (Text, Location)
unaryMinus = do
    (_, loc) <- operator "-"
    return ("-_", loc)

makeUnaryOp :: (Text, Location) -> Expression -> Expression
makeUnaryOp (opName, loc) a =
    Apply (Variable (Name opName auto loc) []) [a]

templateString :: Parser Expression
templateString = do
    TemplateStringBegin <- next
    stringsAndExpressions <- many (eitherP templateStringMid expr)
    TemplateStringEnd <- next
    let parameter1 = ArrayExpression (lefts stringsAndExpressions)
    let parameter2 = ArrayExpression (rights stringsAndExpressions)
    return (Apply (Variable (Name "format" auto emptyLocation) []) [parameter1, parameter2])

templateStringMid :: Parser Expression
templateStringMid = do
    TemplateStringMid s <- next
    return (Literal (StringLiteral s))


literal :: Parser Literal
literal = fmap Int32 integer
    <|> fmap UInt32 unsignedInteger
    <|> fmap Int64 long
    <|> fmap UInt64 unsignedLong
    <|> fmap Float64 double
    <|> fmap StringLiteral string
    <|> bool

arrayExpression :: Parser Expression
arrayExpression = fmap ArrayExpression (squares (sepByTrailing expr (token ",")))

-- Trick to parse left recursive
postfixExpression :: Parser Expression
postfixExpression = liftA2 (foldl (\e f -> f e))
    primaryExpression
    (many (squareAccess <|> dotAcces <|> parameterList))

-- Could be something like
-- `liftA2 (\e f -> f e) postfixExpression parameterList`
-- but postfixExpression consumes everything
statementExpression :: Parser Expression
statementExpression = postfixExpression

primaryExpression :: Parser Expression
primaryExpression =
    fmap Literal literal <|> variable <|> parens expr <|> arrayExpression

-- a.b
dotAcces :: Parser (Expression -> Expression)
dotAcces = liftA2 (\(i, loc) ts e -> DotAccess e (Name i auto loc) ts)
    (token "." *> identifierWithLocation)
    (option [] typeParameters)

-- a[2]
-- a[2 + a]
squareAccess :: Parser (Expression -> Expression)
squareAccess = fmap (flip SquareAccess) (squares expr)

-- a(2, 3)
-- a<int>(2, 3)
parameterList :: Parser (Expression -> Expression)
parameterList = fmap (flip Apply) (parens (sepByTrailing expr (token ",")))

variable :: Parser Expression
variable =
    liftA2 (\(i, loc) ts -> Variable (Name i auto loc) ts) identifierWithLocation (option [] typeParameters)

-- helper functions to transform tokens to values
integer :: Parser Int32
integer = do
    Int n <- next
    return n

unsignedInteger :: Parser Word32
unsignedInteger = do
    UInt n <- next
    return n

long :: Parser Int64
long = do
    Long n <- next
    return n

unsignedLong ::Parser Word64
unsignedLong = do
    ULong n <- next
    return n

double :: Parser Double
double = do
    Double n <- next
    return n

string :: Parser Text
string = do
    String s <- next
    return s

bool :: Parser Literal
bool = (token "true" $> Bool True) <|> (token "false" $> Bool False)

identifierWithLocation :: Parser (Text, Location)
identifierWithLocation = do
    Identifier i loc <- next
    return (i, loc)

operator :: Text -> Parser (Text, Location)
operator t = do
    Special op loc <- next
    if op == t
        then return (op, loc)
        else fail "operator"

identifier :: Parser Text
identifier = do
    Identifier i _ <- next
    return i

token :: Text -> Parser Text
token t = do
    Special s _ <- next
    if s == t then return t else fail "token"

parens :: Parser a -> Parser a
parens p = token "(" *> p <* token ")"

squares :: Parser a -> Parser a
squares p = token "[" *> p <* token "]"

curlies :: Parser a -> Parser a
curlies p = token "{" *> p <* token "}"

angles :: Parser a -> Parser a
angles p = token "<" *> p <* token ">"

sepByTrailing :: Alternative f => f a -> f sep -> f [a]
sepByTrailing p sep = sepBy p sep <* optional sep
