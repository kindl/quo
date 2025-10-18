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

data LocatedText = LocatedText {
    getText :: Text,
    getLocation :: Location
} deriving (Show, Data, Typeable)

type ArraySize = Int32

data Type =
    Concrete LocatedText [Type]
    | ArrayType Type (Maybe ArraySize)
    | FunctionType ReturnType [Type]
    | PointerType Type
        deriving (Show, Data, Typeable)

makePrimitiveType :: Text -> Type
makePrimitiveType t =
    Concrete (LocatedText t emptyLocation) []

auto :: Type
auto = makePrimitiveType "auto"

voidType :: Type
voidType = makePrimitiveType "void"

boolType :: Type
boolType = makePrimitiveType "bool"

stringType :: Type
stringType = makePrimitiveType "string"

charType :: Type
charType = makePrimitiveType "char"

shortType :: Type
shortType = makePrimitiveType "short"

ushortType :: Type
ushortType = makePrimitiveType "ushort"

intType :: Type
intType = makePrimitiveType "int"

uintType :: Type
uintType = makePrimitiveType "uint"

longType :: Type
longType = makePrimitiveType "long"

ulongType :: Type
ulongType = makePrimitiveType "ulong"

floatType :: Type
floatType = makePrimitiveType "float"

doubleType :: Type
doubleType = makePrimitiveType "double"

nullptrType :: Type
nullptrType = PointerType voidType

usizeType :: Type
usizeType = makePrimitiveType "usize"

isPrimitive :: Type -> Bool
isPrimitive ty = elem ty [boolType, charType,
    shortType, ushortType, intType, uintType,
    longType, ulongType, floatType, doubleType,
    usizeType]

instance Eq Type where
    (==) (Concrete n1 ps1) (Concrete n2 ps2) =
        getText n1 == getText n2 && ps1 == ps2
    (==) (PointerType ty1) (PointerType ty2) =
        ty1 == ty2
    (==) (FunctionType returnTy1 tys1) (FunctionType returnTy2 tys2) =
        returnTy1 == returnTy2 && tys1 == tys2
    (==) (ArrayType ty1 maybeSize1) (ArrayType ty2 maybeSize2) =
        ty1 == ty2 && maybeSize1 == maybeSize2
    (==) _ _ = False

isPointerType :: Type -> Bool
isPointerType (PointerType _) = True
isPointerType ty = ty == stringType

isFunctionType :: Type -> Bool
isFunctionType (FunctionType _ _) = True
isFunctionType _ = False

makeFunctionType :: ReturnType -> [Parameter] -> Type
makeFunctionType returnType parameters =
    FunctionType returnType (fmap getType parameters)

makeType :: LocatedText -> [ReturnType] -> Type
makeType (LocatedText "Fn" _) typeParameters =
    FunctionType (last typeParameters) (init typeParameters)
makeType (LocatedText "Pointer" _) [typeParameter] =
    PointerType typeParameter
makeType locatedText typeParameters =
    Concrete locatedText typeParameters

isOperator :: Text -> Bool
isOperator x = elem x operators

operators :: [Text]
operators = ["==", "<=", ">=", "!=", "&&", "||", "-_",
    "!", "^", "?", ":", "+", "-", "*", "/", "%", "<", ">", "="]


data Name = Name {
    getLocatedText :: LocatedText,
    getType :: Type
} deriving (Show, Data, Typeable)

getInnerText :: Name -> Text
getInnerText = getText . getLocatedText

-- Consider adding (Maybe Expression) for default parameters
type Parameter = Name

type TypeParameter = LocatedText

type ReturnType = Type

data Module = Module LocatedText [Statement]
    deriving (Show, Data, Typeable)

data Statement =
    Definition Name Expression
    | FunctionDefintion LocatedText [TypeParameter] ReturnType [Parameter] [Statement]
    | ExternDefintion LocatedText ReturnType [Parameter]
    | StructDefinition LocatedText [TypeParameter] [Parameter]
    | Call Expression
    | Assignment Expression Expression
    | If [(Expression, [Statement])] (Maybe [Statement])
    | Return (Maybe Expression)
    | Import LocatedText (Maybe [LocatedText])
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
