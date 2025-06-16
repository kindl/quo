{-# LANGUAGE OverloadedStrings #-}
module Helpers where

import Data.Text(Text, replace)
import Prettyprinter(Doc, pretty, line, vcat, indent, layoutPretty, defaultLayoutOptions)
import Prettyprinter.Render.Text(renderStrict)


-- TODO check if \0 escpae character works in QBE
escape = replace "\0" "\\0"
    . replace "\n" "\\n"
    . replace "\t" "\\t"
    . replace "\r" "\\r"
    . replace "\"" "\\\""
    . replace "\'" "\\\'"
    . replace "\\" "\\\\"


{- Helper functions for pretty printing -}
fromText :: Text -> Doc a
fromText = pretty

intercalate _ [] = mempty
intercalate "\n" ls = vcat ls
intercalate "\n\n" ls = foldr1 (\l1 l2 -> l1 <//> mempty <//> l2) ls
intercalate sep ls = foldr1 (\l1 l2 -> l1 <> fromText sep <> l2) ls

indentLevel = 4

indent = Prettyprinter.indent indentLevel

infixr 6 <//>

(<//>) x y = x <> line <> y

toText d = renderStrict (layoutPretty defaultLayoutOptions d)