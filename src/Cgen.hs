{-# LANGUAGE OverloadedStrings #-}
module Cgen where

import Types
import Data.Text(pack)
import Drucker(fromText, intercalate, (<+>), (<//>), indent)


toC (Module _ s) = intercalate "\n\n" (fmap statementToC s)


statementToC (Definition name e) =
    nameToC name <+> "=" <+> expressionToC e <> ";"
statementToC (Call e) = expressionToC e <> ";"
statementToC (Assignment e1 e2) = expressionToC e1 <+> "=" <+> expressionToC e2 <> ";"
statementToC (FunctionDefintion name [] ty parameters statements) =
    typeToC ty <+> fromText name
        <> parens (intercalate ", " (fmap nameToC parameters))
        <//> "{"
        <//> indent (intercalate "\n" (fmap statementToC statements))
        <//> "}"
statementToC (Return (Just e)) = "return" <+> expressionToC e <> ";"
statementToC (Return Nothing) = "return;"
statementToC (Import _ _) = ""
statementToC (ExternDefintion name returnType parameters) =
    typeToC returnType <+> fromText name <> parens (intercalate ", " (fmap nameToC parameters)) <>";"
statementToC (StructDefinition name [] parameters) =
    "typedef struct {"
        <//> indent (intercalate "\n" (fmap fieldToC parameters))
        <//> "}" <+> fromText name <> ";"
statementToC (If (cond:conds) Nothing) =
    printIf cond conds
statementToC (If (cond:conds) (Just th)) =
    printIf cond conds <//> printElsePart th
statementToC (While cond sts) = "while" <+> parens (expressionToC cond)
    <//> printBlock sts
statementToC other = error ("Error: CsS Following statement appearedd in printing stage " ++ show other)

printIf cond conds = intercalate "\n" (printIfPart cond : fmap printElseIfPart conds)

printIfPart (cond, sts) = "if" <+> parens (expressionToC cond)
    <//> printBlock sts

printElseIfPart cond = "else" <+> printIfPart cond

printElsePart sts = "else" <//> printBlock sts

printBlock sts = "{"
    <//> indent (intercalate "\n" (fmap statementToC sts))
    <//> "}"

-- Array size specifier come after the identifier in C
nameToC (Name name (ArrayType ty maybeSize)) =
    typeToC ty <+> fromText name <> (case maybeSize of
        Nothing -> "*"
        Just size -> "[" <> fromText (pack (show size)) <> "]")
nameToC (Name name ty) =
    typeToC ty <+> fromText name

paramToC (e, ty) = typeToC ty <+> expressionToC e

fieldToC name = nameToC name <> ";"

expressionToC (Variable name []) = fromName name
expressionToC (Literal l) = literalToC l
-- Operators
expressionToC (Apply (Variable (Name v _) _) [e1, e2]) | isOperator v =
    parensWrapped e1 <+> fromText v <+> parensWrapped e2
-- Non Operator Apply
expressionToC (Apply e es) = expressionToC e <> parens (intercalate ", " (fmap expressionToC es))
expressionToC (DotAccess e name []) =
    expressionToC e <> "." <> fromName name
expressionToC (SquareAccess e1 e2) =
    expressionToC e1 <> "[" <> expressionToC e2 <> "]"
expressionToC (ArrayExpression es) =
    "[" <> intercalate ", " (fmap expressionToC es) <> "]"

fromName (Name n _) = fromText n

literalToC (Int32 l) = fromText (pack (show l))
literalToC (Int64 l) = fromText (pack (show l)) <> "L"
literalToC (Float32 l) = fromText (pack (show l)) <> "f"
literalToC (Float64 l) = fromText (pack (show l))
literalToC (Bool True) = "true"
literalToC (Bool False) = "false"
literalToC (StringLiteral s) = "\"" <> fromText (escape s) <> "\""

-- Possibly adds parens to avoid wrong precedence for operators
parensWrapped e =
    case e of
        (Apply (Variable (Name i _) _) _) | isOperator i -> parens (expressionToC e)
        _ -> expressionToC e

typeToC (PointerType t) = typeToC t <> "*"
typeToC (Concrete s []) = fromText s
typeToC other = error ("Cannot print type of " ++ show other)

parens x = "(" <> x <> ")"
