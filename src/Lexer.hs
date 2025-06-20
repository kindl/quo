{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Lexer(lexe, Token(..)) where

import Prelude hiding (takeWhile)
import Data.Char(isSpace, isAlpha, isAlphaNum, isDigit)
import Types(operators)
import Control.Applicative((<|>), optional)
import Data.Attoparsec.Combinator(manyTill', many', eitherP)
import Data.Attoparsec.Text(takeWhile, takeWhile1, string, char, satisfy,
    anyChar, parseOnly, match, option)
import qualified Data.Text as Text
import Data.Text(Text)
import Data.Int(Int64, Int32)
import Data.Data(Data, Typeable)


data Token =
    Identifier Text
    | Int Int32
    | Long Int64
    | Float Float
    | Double Double
    | Special Text
    | String Text
    | TemplateStringBegin
    | TemplateStringMid Text
    | TemplateStringEnd
    | Whitespace
        deriving (Eq, Show, Data, Typeable)


lexe str = parseOnly (goToTheEnd str lexemes) str

goToTheEnd input p = do
    (parsed, result) <- match p
    let (lineNumber, characterNumber) = Text.foldl' advance (1, 1) parsed
    if Text.length parsed == Text.length input
        then return (filter (not . isWhitespace) result)
        else fail ("Lexer failed at " ++ show lineNumber ++ ":" ++ show characterNumber)

lexemes = fmap collapse (many' lexeme)

isWhitespace Whitespace = True
isWhitespace _ = False

advance (l, _) '\n' = (l + 1, 1)
advance (l, c) _ = (l, c + 1)

whitespace = Whitespace <$ (takeWhile1 isSpace <|> multilineComment <|> singlelineComment)

multilineComment = fmap Text.pack (string "/*" *> manyTill' anyChar (string "*/"))

singlelineComment = string "//" *> takeWhile (/='\n')

-- token
lexeme = eitherP (whitespace <|> singleLexeme) templateString

collapse = foldr collapseAux []

collapseAux (Left l) es = l : es
collapseAux (Right r) es = r ++ es

singleLexeme = fmap String nonTemplateString
    -- Numbers have to come before special,
    -- so that the minus sign becomes part of the number
    <|> numberToken
    <|> fmap Special special
    <|> fmap identifierOrKeyword identifier

digits = takeWhile1 isDigit

digitsWithOptionalSign = do
    s <- option "" (string "+" <> string "-")
    ds <- digits
    return (s <> ds)

exponentPart = do
    e <- char 'E' <> char 'e'
    ds <- digitsWithOptionalSign
    return (Text.cons e ds)

dotPart = do
    d <- char '.'
    ds <- digits
    return (Text.cons d ds)

specifier = char 'f' <|> char 'L'

numberToken = do
    digs <- digitsWithOptionalSign
    dot <- option "" dotPart
    expo <- option "" exponentPart
    spec <- optional specifier
    let combined = Text.unpack (digs <> dot <> expo)

    return (case spec of
        Nothing -> if dot == "" then Int (read combined) else Double (read combined)
        Just 'f' -> Float (read combined)
        Just 'L' -> Long (read combined)
        _ -> error ("Unknown spec " ++ show spec))

-- values
identifier = do
    firstLetter <- satisfy (\x -> isAlpha x || x == '_')
    rest <- takeWhile (\x -> isAlphaNum x || x == '_')
    return (Text.cons firstLetter rest)

identifierOrKeyword i =
    if elem i keywords then Special i else Identifier i

nonTemplateString =
    char '\"' *> escapedString (\x -> x /= '\"' && x /= '\\') <* char '\"'

escapeSequence = char '\\' *>
    (char '\\' <|> char '\"'
    <|> char 'n' *> return '\n'
    <|> char 'r' *> return '\r'
    <|> char 't' *> return '\t'
    <|> char '0' *> return '\0')

templateString = do
    _ <- string "$\""
    midParts <- many' (eitherP templateStringPart expressionPart)
    _ <- char '\"'
    return (TemplateStringBegin : collapse midParts ++ [TemplateStringEnd])

templateStringPart = fmap TemplateStringMid (escapedString (\x -> x /= '\"' && x == '\\' && x /= '{'))

escapedString p = fmap Text.concat (many' (takeWhile1 p <|> fmap Text.singleton escapeSequence))

expressionPart = char '{' *> fmap collapse (manyTill' lexeme (char '}'))

-- special characters
special = foldr1 (<|>) (fmap string specials)

-- beware that the order matters. == and => has to come before =
specials = punctuation ++ operators ++ brackets

keywords = ["fn", "if", "else", "return", "let", "true", "false",
    "for", "in", "switch", "case", "struct", "break", "continue", "while", "extern", "auto",
    "module", "import"]

punctuation = [".", ";", ",", "=>"]
brackets = ["(", ")", "[", "]", "{", "}"]
