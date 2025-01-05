{-# LANGUAGE OverloadedStrings #-}
module Lexer(lexe) where

import Prelude hiding (takeWhile)
import Data.Char(isSpace, isAlpha, isAlphaNum)
import Types
import Control.Applicative((<|>), liftA2)
import Data.Attoparsec.Combinator(manyTill', many')
import Data.Attoparsec.Text(takeWhile, takeWhile1, string, char, satisfy,
    signed, decimal, anyChar, parseOnly, match)
import qualified Data.Text as Text


lexe str = parseOnly (goToTheEnd str lexemesOrWhitespace) str

goToTheEnd input p = do
    (parsed, result) <- match p
    let (lineNumber, characterNumber) = Text.foldl' advance (1, 1) parsed
    if Text.length parsed == Text.length input
        then return result
        else fail ("Parsed failed at " ++ show lineNumber ++ ":" ++ show characterNumber)

lexemesOrWhitespace = fmap concat (many' (whitespace <|> lexeme))


advance (l, _) '\n' = (l + 1, 1)
advance (l, c) _ = (l, c + 1)

whitespace =  ([] <$ takeWhile1 isSpace) <|> ([] <$ multilineComment) <|> ([] <$ singlelineComment)

multilineComment = string "/*" *> manyTill' anyChar (string "*/")

singlelineComment = string "//" *> takeWhile (/='\n')

-- token
lexeme = templateString
    <|> fmap (return . NonTemplateString) nonTemplateString
    <|> fmap (return . Integer) (signed decimal)
    <|> fmap (return . Special) special
    <|> fmap (return . identifierOrKeyword) identifier

-- values
identifier = do
    firstLetter <- satisfy (\x -> isAlpha x || x == '_')
    rest <- takeWhile (\x -> isAlphaNum x || x == '_')
    return (Text.cons firstLetter rest)

identifierOrKeyword i =
    if elem i keywords then Special i else Identifier i

nonTemplateString =
    char '\"' *> fmap Text.concat (many' (takeWhile1 (\x -> x /='"' && x /= '\\') <|> fmap Text.singleton escapeSequence)) <* char '\"'

escapeSequence = char '\\' *>
    (char '\\' <|> char '\"'
    <|> char 'n' *> return '\n'
    <|> char 'r' *> return '\r'
    <|> char 't' *> return '\t'
    <|> char '0' *> return '\0')

-- TODO lexing inside template string
templateString = do
    _ <- string "$\""
    begin <- templateStringPart
    midParts <- many' (liftA2 (\x y -> x ++ [y]) expressionPart templateStringPart)
    _ <- char '"'
    return (TemplateStringBegin : begin : concat midParts ++ [TemplateStringEnd])

templateStringPart = fmap TemplateStringMid (takeWhile (\x -> x /= '"' && x /= '{'))

expressionPart = string "{" *> lexemesOrWhitespaceTill (char '}')

lexemesOrWhitespaceTill p = fmap concat (manyTill' (whitespace <|> lexeme) p)

-- special characters
special = foldr1 (<|>) (fmap string specials)

-- beware that the order matters. == and => has to come before =
specials = punctuation ++ operators ++ brackets

keywords = ["fn", "if", "else", "return", "let", "true", "false",
    "for", "in", "switch", "case", "struct", "break", "continue", "while", "extern", "auto"]

punctuation = [".", ";", ",", "=>"]
brackets = ["(", ")", "[", "]", "{", "}"]
