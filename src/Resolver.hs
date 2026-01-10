{-# LANGUAGE OverloadedStrings #-}
module Resolver(StructLookup, TypeLookup, runResolve, literalType, readType,
    substitute, zipTypeParameters, gatherStructs) where

import Types
import Control.Monad.Trans.Reader(ReaderT, runReaderT, asks, local)
import Control.Monad(zipWithM, when)
import Data.Text(Text)
import Platte(transformBi)
import Data.Maybe(fromMaybe)
import Helpers(find)
import Data.Data(Data)


type TypeLookup = [(Text, Type)]

type StructLookup = [(Text, TypeLookup)]

data Env = Env TypeLookup StructLookup

type Resolve a = ReaderT Env IO a

-- TODO handle operations on non matching types
-- for example should the following be allowed or require an explicit conversion:
-- 1 == 1.0
-- 1 + 1.0
baseEnv :: Env
baseEnv =
    Env [
        -- Equality
        ("==", FunctionType boolType [auto, auto]),
        ("!=", FunctionType boolType [auto, auto]),
        -- Comparisons
        ("<", FunctionType boolType [auto, auto]),
        (">", FunctionType boolType [auto, auto]),
        (">=", FunctionType boolType [auto, auto]),
        ("<=", FunctionType boolType [auto, auto]),
        -- Logical
        ("!", FunctionType boolType [boolType]),
        ("||", FunctionType boolType [boolType, boolType]),
        ("&&", FunctionType boolType [boolType, boolType]),
        -- Bitwise
        ("~", FunctionType auto [auto]),
        ("^", FunctionType auto [auto, auto]),
        ("|", FunctionType auto [auto, auto]),
        ("&", FunctionType auto [auto, auto]),
        -- Arithmetic
        ("-_", FunctionType auto [auto]),
        ("+", FunctionType auto [auto, auto]),
        ("-", FunctionType auto [auto, auto]),
        ("/", FunctionType auto [auto, auto]),
        ("*", FunctionType auto [auto, auto]),

        ("nullptr", nullptrType)
    ] []

runResolve :: Module -> IO Module
runResolve m = runReaderT (resolveModule m) baseEnv

-- for a module:
-- gather the types of all function definitions and extern function definitions
-- gather the structs to bring "constructors" in scope
-- resolve all definitions, they need to be in order
-- resolve all function definitions, they do not need to be in order, because we have all necessary types from annotations
resolveModule :: Module -> Resolve Module
resolveModule (Module name statements) =
    let
        annotations = gatherAnnotations statements
        structs = gatherStructs statements
    in do
        resolved <- withAnnotationsAndStructs annotations structs (resolveDefinitions statements)
        return (Module name resolved)

resolveDefinitions :: [Statement] -> Resolve [Statement]
resolveDefinitions = foldr (resolveDefinition resolveTop) (return mempty)

resolveTop :: Statement -> Resolve Statement
resolveTop (FunctionDefintion text typeParameters returnType parameters body) = do
    resolved <- with (fmap nameToPair parameters) (resolveStatements body)
    let returnTypes = fmap getReturnType (getReturns resolved)
    when (null returnTypes && returnType /= voidType)
        (fail ("Function " ++ show text ++ " has return type " ++ show returnType
            ++ " but does not have a return statement"))

    when (not (all (\x -> subsumes x returnType) returnTypes))
        (fail ("Function " ++ show text ++ " return types " ++ show returnTypes
            ++ " do not fit annotated return type " ++ show returnType))

    return (FunctionDefintion text typeParameters returnType parameters resolved)
resolveTop d@(ExternDefintion _ _ _) = return d
resolveTop d@(StructDefinition _ _ _) = return d
resolveTop i@(Import _ _) = return i
resolveTop statement =
    fail ("Cannot resolve show " ++ show statement)

getReturnType :: Maybe Expression -> Type
getReturnType (Just e) = readType e
getReturnType Nothing = voidType

getReturns :: [Statement] -> [Maybe Expression]
getReturns s = foldMap getReturn s

getReturn :: Statement -> [Maybe Expression]
getReturn (Return e) = [e]
getReturn (If branches maybeElseBranch) =
    foldMap (getReturns . snd) branches ++ getReturns (concat maybeElseBranch)
getReturn (While _ statements) =
    getReturns statements
getReturn (For _ _ statements) =
    getReturns statements
getReturn _ = []

getVariableEnv :: Resolve TypeLookup
getVariableEnv = asks (\(Env e _) -> e)

getStructEnv :: Resolve StructLookup
getStructEnv = asks (\(Env _ e) -> e)

with :: TypeLookup -> ReaderT Env m a -> ReaderT Env m a
with env = local (\(Env variableEnv structEnv) -> Env (variableEnv <> env) structEnv)

withAnnotationsAndStructs :: TypeLookup -> StructLookup -> ReaderT Env m a -> ReaderT Env m a
withAnnotationsAndStructs e s = local (\(Env variableEnv structEnv) -> Env (variableEnv <> e) (structEnv <> s))


resolveStatements :: [Statement] -> Resolve [Statement]
resolveStatements = foldr (resolveDefinition resolveStatement) (return mempty)

resolveDefinition :: (Statement -> Resolve Statement) -> Statement -> Resolve [Statement] -> Resolve [Statement]
resolveDefinition _ (Definition (Name name annotatedType) value) rest = do
    resolved <- resolveExpression value annotatedType
    -- If the type was inferred, replace with the result type, otherwise leave it
    let newType = if annotatedType == auto then readType resolved else annotatedType
    rest' <- with (entry name newType) rest
    return (Definition (Name name newType) resolved : rest')
resolveDefinition f statement rest = do
    resolved <- f statement
    rest' <- rest
    return (resolved:rest')

resolveStatement :: Statement -> Resolve Statement
resolveStatement (Return maybeExpression) =
    fmap Return (traverse (\e -> resolveExpression e auto) maybeExpression)
resolveStatement (Call expression) =
    fmap Call (resolveExpression expression auto)
resolveStatement (Assignment leftHand rightHand) = do
    resolvedExpression <- resolveExpression leftHand auto
    let ty = readType resolvedExpression
    fmap (Assignment resolvedExpression) (resolveExpression rightHand ty)
resolveStatement (If branches elseBranch) = do
    resolvedOptions <- traverse (resolveBranch boolType) branches
    resolvedElseBranch <- traverse resolveStatements elseBranch
    return (If resolvedOptions resolvedElseBranch)
resolveStatement (While condition statements) =
    liftA2 While (resolveExpression condition boolType) (resolveStatements statements)
resolveStatement (For (Name name annotatedType) expression statements) = do
    resolvedExpression <- resolveExpression expression (ArrayType annotatedType Nothing)
    let (ArrayType elementType _) = readType resolvedExpression
    resolvedStatements <- with (entry name elementType) (resolveStatements statements)
    return (For (Name name elementType) resolvedExpression resolvedStatements)
resolveStatement (Switch expression branches) = do
    resolvedExpression <- resolveExpression expression auto
    let conditionType = readType resolvedExpression
    resolvedBranches <- traverse (resolveBranch conditionType) branches
    return (Switch resolvedExpression resolvedBranches)
resolveStatement BreakStatement = return BreakStatement
resolveStatement ContinueStatement = return ContinueStatement
resolveStatement statement =
    fail ("Unexpected definition statement " ++ show statement)

resolveBranch :: Type -> (Expression, [Statement]) -> Resolve (Expression, [Statement])
resolveBranch conditionType (condition, statements) =
    liftA2 (,) (resolveExpression condition conditionType) (resolveStatements statements)

nameToPair :: Name -> (Text, Type)
nameToPair name = (getInnerText name, getType name)

entry :: LocatedText -> Type -> TypeLookup
entry text value = [(getText text, value)]

gatherAnnotations :: [Statement] -> TypeLookup
gatherAnnotations = foldMap gatherAnnotation

gatherAnnotation :: Statement -> TypeLookup
gatherAnnotation (FunctionDefintion text [] returnType parameters _) =
    entry text (makeFunctionType returnType parameters)
gatherAnnotation (StructDefinition text [] parameters) =
    -- gather structs as constructors
    entry text (makeFunctionType (Concrete text []) parameters)
gatherAnnotation (ExternDefintion text returnType parameters) =
    entry text (makeFunctionType returnType parameters)
gatherAnnotation _ = mempty

gatherStructs :: [Statement] -> StructLookup
gatherStructs = foldMap gatherStruct

gatherStruct :: Statement -> StructLookup
gatherStruct (StructDefinition text [] parameters) =
    [(getText text, fmap nameToPair parameters)]
gatherStruct _ = mempty

readType :: Expression -> Type
readType (Variable name _) = getType name
readType (DotAccess _ name _) = getType name
readType (Apply e _) = case readType e of
    FunctionType returnType _ -> returnType
    ty -> error ("Cannot read type of expression " ++ show e ++ " which applies " ++ show ty)
readType (Literal l) = literalType l
readType (SquareAccess expression _) =
    case readType expression of
        ArrayType ty _ -> ty
        ty -> error ("Cannot read type because an array type was expected for " ++ show expression ++ " but was " ++ show ty)
readType (ArrayExpression []) = error "Cannot read type of empty array"
readType (ArrayExpression expressions) =
    let
        ty = readType (head expressions)
    in ArrayType ty (Just (fromIntegral (length expressions)))
readType (IfExpression _ _ e) =
    readType e

literalType :: Literal -> Type
literalType (StringLiteral _) = stringType
literalType (Bool _) = boolType
literalType (Int32 _) = intType
literalType (UInt32 _) = uintType
literalType (Int64 _) = longType
literalType (UInt64 _) = ulongType
literalType (Float32 _) = floatType
literalType (Float64 _) = doubleType

resolveExpression :: Expression -> Type -> Resolve Expression
resolveExpression (Variable var [ty]) _ | getInnerText var == "sizeof" =
    -- Checking the expected type here is mostly not necessary,
    -- because functions are checked against auto first anyway
    let name = getLocatedText var
    in return (Variable (Name name (FunctionType usizeType [])) [ty])
resolveExpression (Variable var []) expectedType = do
    let name = getLocatedText var
    env <- getVariableEnv
    ty <- find name env
    if subsumes ty expectedType
        then return (Variable (Name name ty) [])
        else fail ("Actual type " ++ show ty ++ " did not match " ++ show expectedType)
resolveExpression (DotAccess expression var []) expectedType = do
    resolved <- resolveExpression expression auto
    let resolvedType = readType resolved
    fields <- findFields resolvedType
    let name = getLocatedText var
    fieldType <- find name fields
    if subsumes fieldType expectedType
        then return (DotAccess resolved (Name name fieldType) [])
        else fail ("Actual type " ++ show fieldType ++ " did not match " ++ show expectedType)
resolveExpression (Apply (Variable var [resultType]) [parameter]) expectedType | getInnerText var == "cast" = do
    resolvedParameter <- resolveExpression parameter auto
    let parameterType = readType resolvedParameter
    let castFunctionType = FunctionType resultType [parameterType]
    if subsumes resultType expectedType
        then return (Apply (Variable (Name (getLocatedText var) castFunctionType) [resultType]) [resolvedParameter])
        else fail ("Cast type " ++ show resultType ++ " did not match expected type " ++ show expectedType)
resolveExpression (Apply expression parameters) expectedType = do
    resolvedFunction <- resolveExpression expression auto
    case readType resolvedFunction of
        FunctionType returnType parameterTypes -> do
            resolvedParameters <- zipParameters resolveExpression parameters parameterTypes
            if returnType == auto || elem auto parameterTypes
                then return (resolveOperator returnType resolvedFunction resolvedParameters)
                else if subsumes returnType expectedType
                    then return (Apply resolvedFunction resolvedParameters)
                    else fail ("Return type " ++ show returnType ++ " did not match " ++ show expectedType)
        other ->
            fail ("Cannot apply expression " ++ show resolvedFunction ++ " without function type " ++ show other)
resolveExpression (SquareAccess expression access) expectedType =
    liftA2 SquareAccess (resolveExpression expression (ArrayType expectedType Nothing)) (resolveExpression access intType)
resolveExpression (ArrayExpression expressions) expectedType =
    case expectedType of
        -- TODO Check size match
        ArrayType elementType size -> fmap ArrayExpression (traverse (\e -> resolveExpression e elementType) expressions)
        other -> fail ("An array always expects an element type, but was given " ++ show other)
-- TODO Improve conversion of int literals and consider size
-- for example, something like
-- `char c = 255;` or `someFloat == 0`
-- are really unambiguous and should work without a cast
-- and something like this
-- `char c = 300;`
-- should just fail, because it is greater than char's max value
resolveExpression e@(Literal l) expectedType =
    let
        ty = literalType l
    in if subsumes ty expectedType || (ty == intType && elem expectedType [intType, shortType, charType])
        then return e
        else fail ("Literal type " ++ show ty ++ " is not subsumed by type " ++ show expectedType)
resolveExpression (IfExpression cond thenBranch elseBranch) expectedType = do
    resolvedCond <- resolveExpression cond boolType
    resolvedThen <- traverse (\e -> resolveExpression e expectedType) thenBranch
    resolvedElse <- resolveExpression elseBranch expectedType
    return (IfExpression resolvedCond resolvedThen resolvedElse)
resolveExpression e ty = fail ("Unhandled expression " ++ show e ++ " of " ++ show ty)

-- TODO handle conversions, for example 1 == 1.0
resolveOperator :: Type -> Expression -> [Expression] -> Expression
resolveOperator returnType (Variable var []) [resolved] =
    let
        name = getLocatedText var
        parameterType = readType resolved
        resolvedReturnType = if returnType == auto then parameterType else returnType
    in Apply (Variable (Name name (FunctionType resolvedReturnType [parameterType])) []) [resolved]
resolveOperator returnType (Variable var []) [resolved1, resolved2] =
    let
        name = getLocatedText var
        parameterType = readType resolved1
        resolvedReturnType = if returnType == auto then parameterType else returnType
    in Apply (Variable (Name name (FunctionType resolvedReturnType [parameterType, parameterType])) []) [resolved1, resolved2]
resolveOperator returnType (Variable var []) [resolved1, resolved2, resolved3] =
    let
        name = getLocatedText var
        parameterType = readType resolved1
        resolvedReturnType = if returnType == auto then parameterType else returnType
    in Apply (Variable (Name name (FunctionType resolvedReturnType [parameterType, parameterType])) []) [resolved1, resolved2, resolved3]
resolveOperator _ other _ =
    error ("Cannot resolve operator " ++ show other)

substitute :: Data a => [(Text, Type)] -> a -> a
substitute substitution m =
    let
        f t@(Concrete name []) =
            fromMaybe t (lookup (getText name) substitution)
        f t = t
    in transformBi f m

zipParameters :: MonadFail m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipParameters f xs ys =
    let
        leftLength = length xs
        rightLength = length ys
    in if leftLength == rightLength
        then zipWithM f xs ys
        else fail ("Expected " ++ show leftLength ++ " parameters, but received " ++ show rightLength)

zipTypeParameters :: [a] -> [b] -> [(a, b)]
zipTypeParameters xs ys =
    let
        leftLength = length xs
        rightLength = length ys
    in if leftLength == rightLength
        then zip xs ys
        else error ("Expected " ++ show leftLength ++ " type parameters, but received " ++ show rightLength)

subsumes :: Type -> Type -> Bool
subsumes a _ | a == auto = error "auto on left hand"
subsumes _ b | b == auto = True
-- TODO find a solution for passing arrays to functions expecting a pointer
--subsumes (ArrayType elementType _) (PointerType pointerType) =
--    subsumes elementType pointerType
subsumes (ArrayType a _) (ArrayType b _) =
    subsumes a b
subsumes a b = a == b

findFields :: Type -> Resolve TypeLookup
findFields (Concrete structName []) = do
    structEnv <- getStructEnv
    find structName structEnv
findFields (PointerType innerTy) =
    findFields innerTy
findFields ty =
    fail ("Resolver: Cannot get field of non-struct type " ++ show ty)
