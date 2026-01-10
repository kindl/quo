{-# LANGUAGE OverloadedStrings #-}
module Cgen(prettyC) where

import Types
import Data.Text(Text, pack)
import Prettyprinter(Doc, (<+>), parens)
import Helpers((<//>),
    intercalate, fromText, fromLocatedText,
    escape, indent, isConstructor)


prettyC :: Module -> Doc ann
prettyC (Module _ s) = preamble <> intercalate "\n\n" (fmap statementToC s)

preamble :: Doc ann
preamble = fromText "// This file was generated with quo\ntypedef unsigned long long size_t;\n\n"

statementToC :: Statement -> Doc ann
statementToC (Definition name e) =
    nameToC name <+> "=" <+> expressionToC e <> ";"
statementToC (Call e) = expressionToC e <> ";"
statementToC (Assignment e1 e2) = expressionToC e1 <+> "=" <+> expressionToC e2 <> ";"
statementToC (FunctionDefintion name [] ty parameters statements) =
    typeToC ty <+> fromLocatedText name
        <> parens (intercalate ", " (fmap nameToC parameters))
        <//> "{"
        <//> indent (intercalate "\n" (fmap statementToC statements))
        <//> "}"
statementToC (Return (Just e)) = "return" <+> expressionToC e <> ";"
statementToC (Return Nothing) = "return;"
statementToC (Import _ _) = ""
statementToC (ExternDefintion name returnType parameters) =
    typeToC returnType <+> fromLocatedText name <> parens (intercalate ", " (fmap nameToC parameters)) <>";"
statementToC (StructDefinition name [] parameters) =
    "struct" <+> fromLocatedText name <+> "{"
        <//> indent (intercalate "\n" (fmap fieldToC parameters))
        <//> "}" <> ";"
statementToC (If (cond:conds) Nothing) =
    printIf cond conds
statementToC (If (cond:conds) (Just th)) =
    printIf cond conds <//> printElsePart th
statementToC (While cond sts) =
    "while" <+> parens (expressionToC cond)
    <//> printBlock sts
statementToC BreakStatement = "break;"
statementToC ContinueStatement = "continue;"
statementToC other = error ("Error in Cgen: Following statement appearedd in printing stage " ++ show other)

printIf :: (Expression, [Statement]) -> [(Expression, [Statement])] -> Doc ann
printIf cond conds = intercalate "\n" (printIfPart cond : fmap printElseIfPart conds)

printIfPart :: (Expression, [Statement]) -> Doc ann
printIfPart (cond, sts) = "if" <+> parens (expressionToC cond)
    <//> printBlock sts

printElseIfPart :: (Expression, [Statement]) -> Doc ann
printElseIfPart cond = "else" <+> printIfPart cond

printElsePart :: [Statement] -> Doc ann
printElsePart sts = "else" <//> printBlock sts

printBlock :: [Statement] -> Doc ann
printBlock sts = "{"
    <//> indent (intercalate "\n" (fmap statementToC sts))
    <//> "}"

-- We mostly have to convert the type together with the name because
-- for example array size specifier come after the identifier in C:
-- int numbers[3]; instead of int[3] numbers;
nameToC :: Name -> Doc ann
nameToC (Name name (ArrayType ty maybeSize)) =
    -- Call recursively to handle array of arrays
    nameToC (Name name ty) <> (case maybeSize of
        Nothing -> "[]"
        Just size -> "[" <> fromText (pack (show size)) <> "]")
nameToC (Name name (FunctionType returnType parameterTypes)) =
    typeToC returnType <+> parens ("*" <+> fromLocatedText name) <> parens (intercalate ", " (fmap typeToC parameterTypes))
nameToC (Name name ty) =
    typeToC ty <+> fromLocatedText name

fieldToC :: Name -> Doc ann
fieldToC name = nameToC name <> ";"

expressionToC :: Expression -> Doc a
expressionToC (Variable name []) = fromName name
expressionToC (Variable name typeParameters) =
    error ("Unexpected variable " ++ show name ++ " with type parameters " ++ show typeParameters)
expressionToC (Literal l) = literalToC l
-- Operators
expressionToC (Apply (Variable name [ty]) [expression]) | getInnerText name == "cast" =
    parens (parens (typeToC ty) <> expressionToC expression)
expressionToC (Apply (Variable name [ty]) []) | getInnerText name == "sizeof" =
    "sizeof" <> parens (typeToC ty)
expressionToC (Apply (Variable name _) es) | isOperator (getInnerText name) =
    case es of
        [e] -> fromName name <> parensWrapped e
        [e1, e2] -> parensWrapped e1 <+> fromName name <+> parensWrapped e2
        [e1, e2, e3] -> parensWrapped e1 <+> "?" <+> parensWrapped e2 <+> ":" <+> parensWrapped e3
        _ -> error ("Operator " <> show name <> " has wrong number of arguments")
expressionToC (Apply (Variable name _) es) | isConstructor name =
    parens ("struct" <+> fromName name)
        <> "{" <> intercalate ", " (fmap expressionToC es) <> "}"
expressionToC (Apply e es) = expressionToC e <> parens (intercalate ", " (fmap expressionToC es))
expressionToC (DotAccess e name []) =
    expressionToC e <> "." <> fromName name
expressionToC (DotAccess _ name typeParameters) =
    error ("Unexpected dot access " ++ show name ++ " with type parameters " ++ show typeParameters)
expressionToC (SquareAccess e1 e2) =
    expressionToC e1 <> "[" <> expressionToC e2 <> "]"
expressionToC (ArrayExpression es) =
    "{" <> intercalate ", " (fmap expressionToC es) <> "}"

fromName :: Name -> Doc a
fromName name = fromText (getInnerText name)

literalToC :: Literal -> Doc a
literalToC (Int32 l) = fromText (pack (show l))
literalToC (UInt32 l) = fromText (pack (show l)) <> "U"
literalToC (Int64 l) = fromText (pack (show l)) <> "L"
literalToC (UInt64 l) = fromText (pack (show l)) <> "UL"
literalToC (Float32 l) = fromText (pack (show l)) <> "f"
literalToC (Float64 l) = fromText (pack (show l))
literalToC (Bool True) = "true"
literalToC (Bool False) = "false"
literalToC (StringLiteral s) = "\"" <> fromText (escape s) <> "\""

-- Possibly adds parens to avoid wrong precedence for operators
parensWrapped :: Expression -> Doc a
parensWrapped e =
    case e of
        Apply (Variable name _) _ | isOperator (getInnerText name) ->
            parens (expressionToC e)
        _ -> expressionToC e

typeToC :: Type -> Doc a
typeToC (PointerType t) =
    typeToC t <> "*"
typeToC (FunctionType returnType parameterTypes) =
    typeToC returnType <+> parens "*" <> parens (intercalate ", " (fmap typeToC parameterTypes))
typeToC (Concrete name []) =
    concreteTypeToC (getText name)
typeToC other =
    error ("Cannot print type of " ++ show other)

concreteTypeToC :: Text -> Doc a
concreteTypeToC "void" = "void"
concreteTypeToC "bool" = "bool"
concreteTypeToC "char" = "char"
concreteTypeToC "short" = "short int"
concreteTypeToC "ushort" = "unsigned short int"
concreteTypeToC "int" = "int"
concreteTypeToC "uint" = "unsigned int"
concreteTypeToC "long" = "long long int"
concreteTypeToC "ulong" = "unsigned long long int"
concreteTypeToC "float" = "float"
concreteTypeToC "double" = "double"
concreteTypeToC "usize" = "size_t"
concreteTypeToC "string" = "char*"
-- TODO we need to deal with opaque pointers, otherwise
-- `Pointer<FILE>` becomes `struct FILE*`instead of `FILE*`
concreteTypeToC s = "struct" <+> fromText s
