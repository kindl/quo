{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Types where

import Data.Text(Text)
import Data.Int(Int32, Int64)
import Data.Word(Word32, Word64)
import Data.Data(Data, Typeable)


data Location = Location {
    startLine :: Word64,
    startColumn :: Word64,
    endLine :: Word64,
    endColumn :: Word64,
    fileName :: Text
} deriving (Eq, Show, Data, Typeable)

emptyLocation :: Location
emptyLocation = Location 0 0 0 0 ""

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

stringType :: Type
stringType = Concrete "string" []

charType :: Type
charType = Concrete "char" []

shortType :: Type
shortType = Concrete "short" []

ushortType :: Type
ushortType = Concrete "ushort" []

intType :: Type
intType = Concrete "int" []

uintType :: Type
uintType = Concrete "uint" []

longType :: Type
longType = Concrete "long" []

ulongType :: Type
ulongType = Concrete "ulong" []

floatType :: Type
floatType = Concrete "float" []

doubleType :: Type
doubleType = Concrete "double" []

nullptrType :: Type
nullptrType = PointerType (Concrete "void" [])

usizeType :: Type
usizeType = Concrete "usize" []

isPrimitive :: Type -> Bool
isPrimitive ty = elem ty [boolType, charType,
    shortType, ushortType, intType, uintType,
    longType, ulongType, floatType, doubleType,
    usizeType]

isPointerType :: Type -> Bool
isPointerType (PointerType _) = True
isPointerType (Concrete "string" []) = True
isPointerType _ = False

isFunctionType :: Type -> Bool
isFunctionType (FunctionType _ _) = True
isFunctionType _ = False

makeFunctionType :: ReturnType -> [Parameter] -> Type
makeFunctionType returnType parameters =
    FunctionType returnType (fmap getType parameters)

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


data Name = Name {
    getText :: Text,
    getType :: Type,
    getLocation :: Location
} deriving (Show, Data, Typeable)

-- Consider adding (Maybe Expression) for default parameters
type Parameter = Name

type TypeParameter = Text

type ReturnType = Type

data Module = Module Text [Statement]
    deriving (Show, Data, Typeable)

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
    | BreakStatement
    | ContinueStatement
        deriving (Show, Data, Typeable)

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
        deriving (Show, Data, Typeable)

data Literal =
    StringLiteral Text
    | Bool Bool
    | Int32 Int32
    | UInt32 Word32
    | Int64 Int64
    | UInt64 Word64
    | Float32 Float
    | Float64 Double
        deriving (Eq, Show, Data, Typeable)
