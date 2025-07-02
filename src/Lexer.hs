{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Lexer(lexe, Token(..)) where

import Prelude hiding (takeWhile)
import Data.Char(isSpace, isAlpha, isAlphaNum, isDigit)
import Types(operators)
import Control.Applicative((<|>), optional)
import Data.Attoparsec.Combinator(manyTill', many', eitherP)
import Data.Attoparsec.Text(Parser, takeWhile, takeWhile1, string, char, satisfy,
    anyChar, parseOnly, match, option)
import qualified Data.Text as Text
import Data.Text(Text, isPrefixOf)
import Data.Int(Int32, Int64)
import Data.Word(Word32, Word64)
import Data.Data(Data, Typeable)


data Token =
    Identifier Text
    | Int Int32
    | UInt Word32
    | Long Int64
    | ULong Word64
    | Float Float
    | Double Double
    | Special Text
    | String Text
    | TemplateStringBegin
    | TemplateStringMid Text
    | TemplateStringEnd
    | Whitespace
        deriving (Eq, Show, Data, Typeable)


lexe :: Text -> Either String [Token]
lexe str = parseOnly (goToTheEnd str lexemes) str

goToTheEnd :: Text -> Parser [Token] -> Parser [Token]
goToTheEnd input p = do
    (parsed, result) <- match p
    let (lineNumber, characterNumber) = Text.foldl' advance (1, 1) parsed
    if Text.length parsed == Text.length input
        then return (filter (not . isWhitespace) result)
        else fail ("Lexer failed at " ++ show lineNumber ++ ":" ++ show characterNumber)

lexemes :: Parser [Token]
lexemes = fmap collapse (many' lexeme)

isWhitespace :: Token -> Bool
isWhitespace Whitespace = True
isWhitespace _ = False

advance :: (Int, Int) -> Char -> (Int, Int)
advance (l, _) '\n' = (l + 1, 1)
advance (l, c) _ = (l, c + 1)

whitespace :: Parser Token
whitespace = Whitespace <$ (takeWhile1 isSpace <|> multilineComment <|> singlelineComment)

multilineComment :: Parser Text
multilineComment = fmap Text.pack (string "/*" *> manyTill' anyChar (string "*/"))

singlelineComment :: Parser Text
singlelineComment = string "//" *> takeWhile (/='\n')

-- token
lexeme :: Parser (Either Token [Token])
lexeme = eitherP (whitespace <|> singleLexeme) templateString

collapse :: [Either a [a]] -> [a]
collapse = foldr collapseAux []

collapseAux :: Either a [a] -> [a] -> [a]
collapseAux (Left l) es = l : es
collapseAux (Right r) es = r ++ es

singleLexeme :: Parser Token
singleLexeme = fmap String nonTemplateString
    -- Numbers have to come before special,
    -- so that the minus sign becomes part of the number
    <|> numberToken
    <|> fmap Special special
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
    if elem i keywords then Special i else Identifier i

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

templateString :: Parser [Token]
templateString = do
    _ <- string "$\""
    midParts <- many' (eitherP templateStringPart expressionPart)
    _ <- char '\"'
    return (TemplateStringBegin : collapse midParts ++ [TemplateStringEnd])

templateStringPart :: Parser Token
templateStringPart = fmap TemplateStringMid (escapedString (\x -> x /= '\"' && x == '\\' && x /= '{'))

escapedString :: (Char -> Bool) -> Parser Text
escapedString p = fmap Text.concat (many' (takeWhile1 p <|> fmap Text.singleton escapeSequence))

expressionPart :: Parser [Token]
expressionPart = char '{' *> fmap collapse (manyTill' lexeme (char '}'))

-- special characters
special :: Parser Text
special = foldr1 (<|>) (fmap string specials)

-- beware that the order matters. == and => has to come before =
specials :: [Text]
specials = punctuation ++ operators ++ brackets

keywords :: [Text]
keywords = ["fn", "if", "else", "return", "let", "true", "false",
    "for", "in", "switch", "case", "struct", "break", "continue", "while", "extern", "auto",
    "module", "import"]

punctuation :: [Text]
punctuation = [".", ";", ",", "=>"]

brackets :: [Text]
brackets = ["(", ")", "[", "]", "{", "}"]
