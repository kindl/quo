{-# LANGUAGE OverloadedStrings #-}
module Helpers where

import Data.Text(Text, replace)
import Prettyprinter(Doc, pretty, line, vcat, indent, layoutPretty, defaultLayoutOptions)
import Prettyprinter.Render.Text(renderStrict)
import Types(Name(Name), Type(Concrete, FunctionType))


-- TODO check if \0 escape character works in QBE
escape :: Text -> Text
escape = replace "\0" "\\0"
    . replace "\n" "\\n"
    . replace "\t" "\\t"
    . replace "\r" "\\r"
    . replace "\"" "\\\""
    . replace "\'" "\\\'"
    . replace "\\" "\\\\"

-- Helper functions for pretty printing
fromText :: Text -> Doc a
fromText = pretty

intercalate :: Text -> [Doc ann] -> Doc ann
intercalate _ [] = mempty
intercalate "\n" ls = vcat ls
intercalate "\n\n" ls = foldr1 (\l1 l2 -> l1 <//> mempty <//> l2) ls
intercalate sep ls = foldr1 (\l1 l2 -> l1 <> fromText sep <> l2) ls

indentLevel :: Int
indentLevel = 4

indent :: Doc ann -> Doc ann
indent = Prettyprinter.indent indentLevel

infixr 6 <//>

(<//>) :: Doc ann -> Doc ann -> Doc ann
(<//>) x y = x <> line <> y

toText :: Doc ann -> Text
toText d = renderStrict (layoutPretty defaultLayoutOptions d)

find name env = case lookup name env of
    Nothing -> fail ("Cannot find name " ++ show name ++ " in env " ++ show env)
    Just found -> return found

isConstructor :: Name -> Bool
isConstructor (Name name (FunctionType (Concrete structName []) _)) =
    name == structName
isConstructor (Name _ _) = False
