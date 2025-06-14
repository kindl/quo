{-# LANGUAGE OverloadedStrings #-}
module Compiler where

import Types
import Data.Text(pack, replace)
import Drucker(fromText, intercalate, (<+>), (<//>), indent)
import Resolver(readType)


-- TODO currently not functional
-- Pretty Printing to Qbe format
toQbe s = intercalate "\n\n" (fmap definitionToQbe s)

-- Top level
definitionToQbe (Definition (Name name ty) e) =
    "data" <+> "$" <> fromText name <+> "=" <+> "{" <+> typeToQbe ty <+> expressionToQbe e <+> "}"
definitionToQbe s = statementToQbe s

-- TODO use type in comparisons for float and similar
statementToQbe (Definition (Name name ty) (Apply (Variable (Name "==" _) _) [e1, e2])) =
    indent ("%" <> fromText name <+> "=" <> typeToQbe ty <+> "ceql" <+> expressionToQbe e1 <> "," <+> expressionToQbe e2)
statementToQbe (Definition (Name name ty) (Apply (Variable (Name "!=" _) _) [e1, e2])) =
    indent ("%" <> fromText name <+> "=" <> typeToQbe ty <+> "cnel" <+> expressionToQbe e1 <> "," <+> expressionToQbe e2)
-- No variable necessary for void
statementToQbe (Definition (Name "_" (Concrete "void" [])) (Apply v parameters)) =
    indent (toQbeCall (readType v) v parameters)
statementToQbe (Definition (Name name returnType) (Apply v parameters)) =
    indent ("%" <> fromText name <+> "=" <> typeToQbe returnType <+> toQbeCall (readType v) v parameters)
statementToQbe (Definition (Name name ty) e) =
    indent ("%" <> fromText name <+> "=" <> typeToQbe ty <+> expressionToQbe e)
statementToQbe (FunctionDefintion name _ ty parameters statements) =
    "export function" <+> typeToQbe ty <+> "$" <> fromText name
    <> parens (intercalate ", " (fmap toQbeFunParam parameters))
    <+> "{"
    <//> "@start"
    <//> intercalate "\n" (fmap statementToQbe statements)
    <//> "}"
statementToQbe (Return (Just e)) = indent ("ret" <+> expressionToQbe e)
statementToQbe (Return Nothing) = indent "ret"
-- statementToQbe (JumpNonZero c t e) = indent ("jnz" <+> "%" <> fromText c <> "," <+> "@" <> fromText t <> "," <+> "@" <> fromText e)
-- statementToQbe (Jump l) = indent ("jmp" <+> "@" <> fromText l)
-- statementToQbe (Label e) = "@" <> fromText e
-- TODO filter these out, because they create a lot of whitespace
statementToQbe (Import _ _) = ""
statementToQbe (ExternDefintion _ _ _) = ""
statementToQbe (StructDefinition name _ parameters) =
    "type" <+> ":" <> fromText name <+> "=" <+> "{"
        <> intercalate ", " (fmap toQbeStructParam parameters)
        <> "}"
statementToQbe other = error ("Error: QbeS Following statement appearedd in printing stage " ++ show other)

toQbeCall (FunctionType _ tys) v parameters =
    let
        parametersWithType = zip parameters tys
    in "call" <+> expressionToQbe v <> parens (intercalate ", " (fmap toQbeParam parametersWithType))
toQbeCall t v _ = error ("Error: Non-function type in toQbeCall " ++ show t ++ " calling " ++ show v)

toQbeFunParam (Name name ty) = typeToQbe ty <+> "%" <> fromText name

toQbeParam (e, ty) = typeToQbe ty <+> expressionToQbe e

-- TODO add size for arrays
-- in C-style the size is after the name
-- int a[5] instead of int[5] a
toQbeStructParam (Name _ ty) = typeToQbe ty

--expressionToQbe (Variable name _) = "%" <> fromText name
--expressionToQbe (Variable name _) = "$" <> fromText name
expressionToQbe (Variable name _) = fromName name
expressionToQbe (Literal l) = literalToQbe l
expressionToQbe other = error ("QbeE " ++ show other)

literalToQbe (Int32 l) = fromText (pack (show l))
literalToQbe (Int64 l) = fromText (pack (show l))
literalToQbe (Float32 l) = fromText (pack (show l))
literalToQbe (Float64 l) = fromText (pack (show l))
literalToQbe (Bool True) = "true"
literalToQbe (Bool False) = "false"
literalToQbe (StringLiteral s) = "\"" <> fromText (escape s) <> "\""

-- TODO check if \0 escpae character works in QBE
escape = replace "\0" "\\0"
    . replace "\n" "\\n"
    . replace "\t" "\\t"
    . replace "\r" "\\r"
    . replace "\"" "\\\""
    . replace "\'" "\\\'"
    . replace "\\" "\\\\"

-- TODO Accessing a struct is just a read at offset
--expressionToQbe (DotAccess e name typeParameters) =
-- TODO Accessing an array is just a read at offset
--expressionToQbe (SquareAccess e1 e2) =
-- TODO Creating an array is just allocating on stack and filling it
--expressionToQbe (ArrayExpression es) =

-- TODO decide between string pointer and string
typeToQbe (Concrete "char" []) = "b"
typeToQbe (Concrete "void" []) = " "
typeToQbe (Concrete "int" []) = "w"
typeToQbe (Concrete "long" []) = "l"
typeToQbe (Concrete "auto" []) = error "Error: auto appeared in printing stage"
typeToQbe (Concrete s []) = ":" <> fromText s
typeToQbe ty =
    error ("Error: generic type " ++ show ty ++ " cannot be turned in a qbe type")



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

typeToC (Concrete "Pointer" [t]) = typeToC t <> "*"
typeToC (Concrete s []) = fromText s
typeToC other = error ("Cannot print type of " ++ show other)

parens x = "(" <> x <> ")"
