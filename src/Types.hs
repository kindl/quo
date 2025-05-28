{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Types where

import Data.Text(Text)
import Data.Int(Int64, Int32)
import Data.Data(Data, Typeable)


type ArraySize = Int32

data Type =
    Concrete Text [Type]
    | ArrayType Type ArraySize
        deriving (Eq, Show, Data, Typeable)

auto = Concrete "auto" []

makeFunctionType returnType argumentTypes =
    Concrete "Fn" (argumentTypes ++ [returnType])

makeFnTypeWithParameters returnType parameters =
    makeFunctionType returnType (fmap (\(Name n t) -> t) parameters)

isOperator x = elem x operators

operators :: [Text]
operators = ["==", "<=", ">=", "!=", "&&", "||",
    "!", "^", "?", ":", "+", "-", "*", "/", "%", "<", ">", "="]

data Name = Name Text Type
    deriving (Eq, Show, Data, Typeable)

-- Consider adding (Maybe Expression) for default parameters
type Parameter = Name

type TypeParameter = Text

type ReturnType = Type

data Module = Module Text [Statement]
    deriving (Eq, Show, Data, Typeable)

data Statement =
    Definition Name Expression
    | FunctionDefintion Text ReturnType [TypeParameter] [Parameter] [Statement]
    | ExternDefintion Text ReturnType [Parameter]
    | StructDefinition Text [TypeParameter] [Parameter]
    | Call Expression
    | Assignment Expression Expression
    | If [(Expression, [Statement])] (Maybe [Statement])
    | Return (Maybe Expression)
    | Import Text (Maybe [Text])
    | For Name Expression [Statement]
    | While Expression [Statement]
    | Switch Expression [(Expression, [Statement])]
    -- These are not parsed, but other statements like While
    -- are lowered into jumps and labels
    -- | Label Text
    -- | Jump Text
    -- | JumpNonZero Text Text Text
        deriving (Eq, Show, Data, Typeable)

-- TODO find the best option to model generics
-- Currently the type parameters are part of Variable and DotAccess
-- It could instead be modeled as a seperate access constructor GenericAccess Expression [Type]
-- or could become part of a Name, so we would have Variable Name and DotAcess Expression Name.
-- Putting it into the name appears clean, but it implies that we have to differentiate between
-- defining names, which only have type variable parameters and names being used, which have more complex types
-- A cast is modeled as application cast<ResultType>(value)
-- the problem with that might be, that it might be missing the input type
data Expression =
    Apply Expression [Expression]
    | Variable Name [Type]
    | DotAccess Expression Name [Type]
    | SquareAccess Expression Expression
    | ArrayExpression [Expression]
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
