{-# LANGUAGE OverloadedStrings #-}
module Drucker where

import Data.String(IsString, fromString)
import Data.Text(Text)


data Document =
    Line Text
    | Newline Document Document
    | Indentation Document
        deriving (Eq, Show)

instance Semigroup Document where
    (<>) (Line text1) (Line text2) = Line (text1 <> text2)
    (<>) l1 (Newline l2 l3) = Newline (l1 <> l2) l3
    (<>) (Newline l1 l2) l3 = Newline l1 (l2 <> l3)
    (<>) (Indentation l1) l2 = Indentation (l1 <> l2)
    (<>) l1@(Line _) l2@(Indentation _) = error ("Can't add: " ++ show l1 ++ " <> " ++ show l2)

instance Monoid Document where
    mempty = Line ""

instance IsString Document where
    fromString = Line . fromString

infixr 6 <+>
infixr 6 <//>

(<//>) = Newline
(<+>) l1 l2 = l1 <> " " <> l2 

fromText = Line

toText = toTextWithIndent 0

spaceLength = 4

nSpaces n = fromString (replicate n ' ')

toTextWithIndent n (Line text) =
    nSpaces n <> text
toTextWithIndent n (Newline line1 line2) =
    toTextWithIndent n line1 <> "\n" <> toTextWithIndent n line2
toTextWithIndent n (Indentation ls) =
    toTextWithIndent (n + spaceLength) ls

indent = Indentation

intercalate _ [] = Line ""
intercalate "\n" ls = foldr1 Newline ls
intercalate "\n\n" ls = foldr1 (\l1 l2 -> l1 <//> mempty <//> l2) ls
intercalate sep ls = foldr1 (\l1 l2 -> l1 <> sep <> l2) ls
