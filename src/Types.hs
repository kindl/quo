{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Types where

import Data.Text(Text)
import Data.Int(Int64, Int32)
import Data.Data(Data, Typeable)


type ArraySize = Int32

data Type = TypeVariable Text [Type] (Maybe ArraySize)
    deriving (Eq, Show, Data, Typeable)

auto = TypeVariable "auto" [] Nothing

isOperator x = elem x operators

operators :: [Text]
operators = ["==", "<=", ">=", "!=", "&&", "||",
    "!", "^", "?", ":", "+", "-", "*", "/", "%", "<", ">", "="]

-- Consider adding (Maybe Expression) for default parameters
type Parameter = (Text, Type)

type TypeParameter = Text

type ReturnType = Type

type FunctionType = Type


data Module = Module Text [Statement]
    deriving (Eq, Show, Data, Typeable)

data Statement =
    Definition Text Type Expression
    | FunctionDefintion Text ReturnType [TypeParameter] [Parameter] [Statement]
    | ExternDefintion Text ReturnType [Parameter]
    | StructDefinition Text [TypeParameter] [Parameter]
    | Call Expression
    | Assignment Expression Expression
    | If [(Expression, [Statement])] (Maybe [Statement])
    | Return (Maybe Expression)
    | Import Text (Maybe [Text])
    | For Text Type Expression [Statement]
    | While Expression [Statement]
    | Switch Expression [(Expression, [Statement])]
    -- These are not parsed, but other statements like While
    -- are lowered into jumps and labels
    | Label Text
    | Jump Text
    | JumpNonZero Text Text Text
        deriving (Eq, Show, Data, Typeable)

data Expression =
    Apply FunctionType Expression [Expression]
    -- TODO is it smarter to model x<t> as a seperate access or keep it in Variable and DotAccess?
    | Variable Text [Type]
    | DotAccess Expression Text [Type]
    | SquareAccess Expression Expression
    | ArrayExpression [Expression]
    -- TODO could CastExpressions modeled as applying
    -- Variable _ "cast" [resultType]
    | CastExpression Type Expression
    | Literal Literal
        deriving (Eq, Show, Data, Typeable)

data Literal =
    StringLiteral Text
    | Bool Bool
    | Int32 Int32
    | Int64 Int64
    | Float32 Float
    | Float64 Double
        deriving (Eq, Show, Data, Typeable)
