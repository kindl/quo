{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Lexer(tokenize, Token(..)) where

import Prelude hiding (takeWhile)
import Data.Char(isSpace, isAlpha, isAlphaNum, isDigit)
import Types(Location(..), emptyLocation, operators)
import Control.Applicative((<|>), optional)
import Data.Attoparsec.Combinator(manyTill', many', eitherP)
import Data.Attoparsec.Text(Parser, takeWhile, takeWhile1, string, char, satisfy,
    anyChar, parseOnly, match, option, endOfInput)
import qualified Data.Text as Text
import Data.Text(Text, isPrefixOf)
import Data.Int(Int32, Int64)
import Data.Word(Word32, Word64)
import Data.Data(Data, Typeable)
import Data.List(mapAccumL)
import Data.Functor(($>))


data Token =
    Identifier Text Location
    -- Special combines keywords and operators,
    -- because in some cases, for example for "<"
    -- a token can be used for both
    | Special Text Location
    | Int Int32
    | UInt Word32
    | Long Int64
    | ULong Word64
    | Float Float
    | Double Double
    | String Text
    | TemplateStringBegin
    | TemplateStringMid Text
    | TemplateStringEnd
    | Whitespace
        deriving (Eq, Show, Data, Typeable)

collapse :: [Either a [a]] -> [a]
collapse = foldr collapseAux []
    where
        collapseAux :: Either a [a] -> [a] -> [a]
        collapseAux (Left l) es = l : es
        collapseAux (Right r) es = r ++ es

tokenize :: Text -> Text -> Either String [Token]
tokenize file input = parseOnly (tokenizeToEnd file) input

tokenizeToEnd :: Text -> Parser [Token]
tokenizeToEnd file = do
    ts <- fmap collapse (many' token <* endOfInput)
    let located = locate file ts
    return (filter (not . isWhitespace) located)

locate :: Traversable t => Text -> t (Text, Token) -> t Token
locate file ts = snd (mapAccumL (\startPosition (parsed, result) ->
    let
        endPosition = Text.foldl' advance startPosition parsed
        location = Location {
            startLine = fst startPosition,
            startColumn = snd startPosition,
            endLine = fst endPosition,
            endColumn = snd endPosition,
            fileName = file
        }
        locatedToken = overwriteLocation location result
    in (endPosition, locatedToken)) (1, 1) ts)

isWhitespace :: Token -> Bool
isWhitespace Whitespace = True
isWhitespace _ = False

advance :: (Word64, Word64) -> Char -> (Word64, Word64)
advance (l, _) '\n' = (l + 1, 1)
advance (l, c) _ = (l, c + 1)

overwriteLocation :: Location -> Token -> Token
overwriteLocation location (Identifier i _) =
    Identifier i location
overwriteLocation location (Special op _) =
    Special op location
overwriteLocation _ other = other

whitespace :: Parser Token
whitespace = Whitespace <$ (takeWhile1 isSpace <|> multilineComment <|> singlelineComment)

multilineComment :: Parser Text
multilineComment = fmap Text.pack (string "/*" *> manyTill' anyChar (string "*/"))

singlelineComment :: Parser Text
singlelineComment = string "//" *> takeWhile (/='\n')

token :: Parser (Either (Text, Token) [(Text, Token)])
token = eitherP (match (whitespace <|> singleToken)) templateString

singleToken :: Parser Token
singleToken = fmap String nonTemplateString
    -- Numbers have to come before special,
    -- so that the minus sign becomes part of the number
    <|> numberToken
    <|> fmap (flip Special emptyLocation) special
    <|> fmap identifierOrKeyword identifier

digits :: Parser Text
digits = takeWhile1 isDigit

digitsWithOptionalSign :: Parser Text
digitsWithOptionalSign = do
    s <- option "" (string "+" <> string "-")
    ds <- digits
    return (s <> ds)

exponentPart :: Parser Text
exponentPart = do
    e <- char 'E' <> char 'e'
    ds <- digitsWithOptionalSign
    return (Text.cons e ds)

dotPart :: Parser Text
dotPart = do
    d <- char '.'
    ds <- digits
    return (Text.cons d ds)

specifier :: Parser Text
specifier = string "f" <|> string "L" <|> string "UL" <|> string "U"

numberToken :: Parser Token
numberToken = do
    digs <- digitsWithOptionalSign
    dot <- option "" dotPart
    expo <- option "" exponentPart
    spec <- optional specifier
    let combined = Text.unpack (digs <> dot <> expo)

    return (case spec of
        Nothing -> if dot == "" then Int (read combined) else Double (read combined)
        Just "f" -> Float (read combined)
        Just "L" -> Long (read combined)
        Just "U" -> if isPrefixOf "-" digs then error "Unsigned literal with sign" else UInt (read combined)
        Just "UL" -> if isPrefixOf "-" digs then error "Unsigned literal with sign" else ULong (read combined)
        _ -> error ("Unknown spec " ++ show spec))

-- values
identifier :: Parser Text
identifier = do
    firstLetter <- satisfy (\x -> isAlpha x || x == '_')
    rest <- takeWhile (\x -> isAlphaNum x || x == '_')
    return (Text.cons firstLetter rest)

identifierOrKeyword :: Text -> Token
identifierOrKeyword i =
    if elem i keywords
        then Special i emptyLocation
        else Identifier i emptyLocation

nonTemplateString :: Parser Text
nonTemplateString =
    char '\"' *> escapedString (\x -> x /= '\"' && x /= '\\') <* char '\"'

escapeSequence :: Parser Char
escapeSequence = char '\\' *>
    (char '\\' <|> char '\"'
    <|> char 'n' *> return '\n'
    <|> char 'r' *> return '\r'
    <|> char 't' *> return '\t'
    <|> char '0' *> return '\0')

templateString :: Parser [(Text, Token)]
templateString = do
    begin <- match (string "$\"" $> TemplateStringBegin)
    midParts <- many' (eitherP (match templateStringPart) expressionPart)
    end <- match (char '\"' $> TemplateStringEnd)
    return (begin : collapse midParts ++ [end])

templateStringPart :: Parser Token
templateStringPart = fmap TemplateStringMid (escapedString (\x -> x /= '\"' && x == '\\' && x /= '{'))

escapedString :: (Char -> Bool) -> Parser Text
escapedString p = fmap Text.concat (many' (takeWhile1 p <|> fmap Text.singleton escapeSequence))

-- TODO ignoring the braces will result in wrong column numbers
expressionPart :: Parser [(Text, Token)]
expressionPart = char '{' *> fmap collapse (manyTill' token (char '}'))

-- special characters
-- beware that the order matters. == and => has to come before =
special :: Parser Text
special =
    foldr1 (<|>)
        (fmap string (punctuation ++ operators ++ brackets))

keywords :: [Text]
keywords = ["fn", "if", "else", "return", "let", "true", "false",
    "for", "in", "switch", "case", "struct", "break", "continue", "while", "extern", "auto",
    "module", "import", "break", "continue"]

punctuation :: [Text]
punctuation = [".", ";", ",", "=>"]

brackets :: [Text]
brackets = ["(", ")", "[", "]", "{", "}"]
