{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Types where

import Data.Text(Text)
import Data.Int(Int64, Int32)
import Data.Data(Data, Typeable)


type ArraySize = Int32

data Type =
    Concrete Text [Type]
    | ArrayType Type (Maybe ArraySize)
    | FunctionType ReturnType [Type]
    | PointerType Type
        deriving (Eq, Show, Data, Typeable)

auto :: Type
auto = Concrete "auto" []

voidType :: Type
voidType = Concrete "void" []

boolType :: Type
boolType = Concrete "bool" []

intType :: Type
intType = Concrete "int" []

longType :: Type
longType = Concrete "long" []

floatType :: Type
floatType = Concrete "float" []

doubleType :: Type
doubleType = Concrete "double" []

nullptrType :: Type
nullptrType = PointerType (Concrete "void" [])


makeFunctionType :: ReturnType -> [Parameter] -> Type
makeFunctionType returnType parameters =
    FunctionType returnType (fmap (\(Name _ t) -> t) parameters)

makeConcrete :: Text -> [ReturnType] -> Type
makeConcrete "Fn" typeParameters =
    FunctionType (last typeParameters) (init typeParameters)
makeConcrete "Pointer" [typeParameter] =
    PointerType typeParameter
makeConcrete name typeParameters =
    Concrete name typeParameters


isOperator :: Text -> Bool
isOperator x = elem x operators

operators :: [Text]
operators = ["==", "<=", ">=", "!=", "&&", "||", "-_",
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
    | FunctionDefintion Text [TypeParameter] ReturnType [Parameter] [Statement]
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
