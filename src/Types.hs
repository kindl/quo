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

-- TODO find the best option to model generics
-- Currently the type parameters are part of Variable and DotAccess
-- It could instead be modeled as a seperate access constructor GenericAccess Expression [Type]
-- or could become part of a Name, so we would have Variable Name and DotAcess Expression Name.
-- Putting it into the name appears clean, but it implies that we have to differentiate between
-- defining names, which only have type variable parameters and names being used, which have more complex types
data Expression =
    Apply Expression [Expression]
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
