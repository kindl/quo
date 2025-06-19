{-# LANGUAGE OverloadedStrings #-}
module Resolver where

import Types
import Control.Applicative(liftA2)
import Control.Monad.Trans.Reader(runReaderT, asks, local, ReaderT)
import Control.Monad(zipWithM, when)
import Data.Text(Text)
import qualified Data.Text as Text
import Platte
import Data.Maybe(fromMaybe)


data Env = Env [(Text, Type)] [(Text, [(Text, Type)])]

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
        -- Bool
        ("!", FunctionType boolType [boolType]),
        ("||", FunctionType boolType [boolType, boolType]),
        ("&&", FunctionType boolType [boolType, boolType]),
        -- Arithmetic
        -- TODO return type
        ("-_", FunctionType auto [auto]),
        ("+", FunctionType auto [auto, auto]),
        ("-", FunctionType auto [auto, auto]),
        ("/", FunctionType auto [auto, auto]),
        ("*", FunctionType auto [auto, auto]),

        ("nullptr", nullptrType)
    ] []

runResolve m = runReaderT (resolveModule m) baseEnv

-- for a module:
-- gather the types of all function definitions and extern function definitions
-- gather the structs to bring "constructors" in scope
-- resolve all definitions, they need to be in order
-- resolve all function definitions, they do not need to be in order, because we have all necessary types from annotations
resolveModule (Module name statements) =
    let
        annotations = gatherAnnotations statements
        structs = gatherStructs statements
    in do
        resolved <- with' annotations structs (resolveDefinitions statements)
        return (Module name resolved)


resolveDefinitions = foldr (resolveDefinition resolveTop) (return mempty)

resolveTop (FunctionDefintion text typeParameters returnType parameters body) = do
    resolved <- with (fmap nameToPair parameters) (resolveStatements body)
    let returnTypes = fmap getReturnType (getReturns resolved)
    when (null returnTypes && returnType /= voidType)
        (fail ("Function " ++ show text ++ " has return type " ++ show returnType ++ " but does not have a return statement"))

    when (not (all (\x -> subsumes x returnType) returnTypes))
        (fail ("Function " ++ show text ++ " return types " ++ show returnTypes ++ " do not fit annotated return type " ++ show returnType))

    return (FunctionDefintion text typeParameters returnType parameters resolved)
resolveTop d@(ExternDefintion _ _ _) = return d
resolveTop d@(StructDefinition _ _ _) = return d
resolveTop i@(Import _ _) = return i
resolveTop statement =
    fail ("Cannot resolve show " ++ show statement)

getReturnType (Just e) = readType e
getReturnType Nothing = voidType

getReturns :: [Statement] -> [Maybe Expression]
getReturns s = foldMap getReturn s

getReturn (Return e) = [e]
getReturn (If branches maybeElseBranch) =
    foldMap (getReturns . snd) branches ++ getReturns (concat maybeElseBranch)
getReturn (While _ statements) =
    getReturns statements
getReturn (For _ _ statements) =
    getReturns statements
getReturn _ = []

getVariableEnv = asks (\(Env e _) -> e)

getStructEnv = asks (\(Env _ e) -> e)

with env = local (\(Env variableEnv structEnv) -> Env (variableEnv <> env) structEnv)

with' e s = local (\(Env variableEnv structEnv) -> Env (variableEnv <> e) (structEnv <> s))


resolveStatements = foldr (resolveDefinition resolveStatement) (return mempty)

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
    resolvedExpression <- resolveExpression expression annotatedType
    let newType = readType resolvedExpression
    resolvedStatements <- with (entry name newType) (resolveStatements statements)
    return (For (Name name newType) resolvedExpression resolvedStatements)
resolveStatement (Switch expression branches) = do
    resolvedExpression <- resolveExpression expression auto
    let conditionType = readType resolvedExpression
    resolvedBranches <- traverse (resolveBranch conditionType) branches
    return (Switch resolvedExpression resolvedBranches)
resolveStatement statement =
    fail ("Unexpected definition statement " ++ show statement)

resolveBranch conditionType (condition, statements) =
    liftA2 (,) (resolveExpression condition conditionType) (resolveStatements statements)


nameToPair (Name n t) = (n, t)

entry text value = [(text, value)]


gatherAnnotations = foldMap gatherAnnotation

gatherAnnotation (FunctionDefintion text [] returnType parameters _) =
    entry text (makeFunctionType returnType parameters)
gatherAnnotation (StructDefinition text [] parameters) =
    -- gather structs as constructors
    entry text (makeFunctionType (Concrete text []) parameters)
gatherAnnotation (ExternDefintion text returnType parameters) =
    entry text (makeFunctionType returnType parameters)
gatherAnnotation _ = mempty

gatherStructs = foldMap gatherStruct

gatherStruct (StructDefinition text [] parameters) =
    [(text, fmap nameToPair parameters)]
gatherStruct _ = mempty

readType :: Expression -> Type
readType (Variable (Name _ ty) _) = ty
readType (DotAccess _ (Name _ ty) _) = ty
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

literalType (StringLiteral l) = ArrayType (Concrete "char" []) (Just (fromIntegral (Text.length l + 1)))
literalType (Bool _) = boolType
literalType (Int32 _) = intType
literalType (Int64 _) = longType
literalType (Float32 _) = floatType
literalType (Float64 _) = doubleType

resolveExpression :: Expression -> Type -> ReaderT Env IO Expression
resolveExpression (Variable (Name name _) []) expectedType = do
    env <- getVariableEnv
    ty <- find name env
    if subsumes ty expectedType
        then return (Variable (Name name ty) [])
        else fail ("Actual type " ++ show ty ++ " did not match " ++ show expectedType)
resolveExpression (DotAccess expression (Name name _) []) expectedType = do
    resolved <- resolveExpression expression auto
    case readType resolved of
        Concrete structName [] -> do
            env <- getStructEnv
            fields <- find structName env
            fieldType <- find name fields
            if subsumes fieldType expectedType
                then return (DotAccess resolved (Name name fieldType) [])
                else fail ("Actual type " ++ show fieldType ++ " did not match " ++ show expectedType)
        -- TODO special case for built-in types like Pointer<T>
        other -> fail ("Cannot access non-struct type" ++ show other)
resolveExpression (Apply expression parameters) expectedType = do
    resolvedFunction <- resolveExpression expression auto
    case readType resolvedFunction of
        FunctionType returnType parameterTypes -> do
            resolvedParameters <- zipParameters resolveExpression parameters parameterTypes
            if returnType == auto
                then return (resolveOperator resolvedFunction resolvedParameters)
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
resolveExpression e@(Literal l) expectedType =
    let
        ty = literalType l
    in if subsumes ty expectedType
        then return e
        else fail ("Literal type " ++ show ty ++ " is not subsumed by type " ++ show expectedType)

-- TODO handle conversions
resolveOperator (Variable (Name name _) []) [resolved] =
    let
        ty = readType resolved
    in Apply (Variable (Name name (FunctionType ty [ty])) []) [resolved]
resolveOperator (Variable (Name name _) []) [resolved1, resolved2] =
    let
        ty = readType resolved1
    in Apply (Variable (Name name (FunctionType ty [ty, ty])) []) [resolved1, resolved2]
resolveOperator other _ =
    error ("Cannot resolve operator " ++ show other)

substitute substitution m =
    let
        f t@(Concrete v []) =
            fromMaybe t (lookup v substitution)
        f t = t
    in transformBi f m

zipParameters f xs ys =
    let
        leftLength = length xs
        rightLength = length ys
    in if leftLength == rightLength
        then zipWithM f xs ys
        else fail ("Expected " ++ show leftLength ++ " parameters, but received " ++ show rightLength)

zipTypeParameters xs ys =
    let
        leftLength = length xs
        rightLength = length ys
    in if leftLength == rightLength
        then zip xs ys
        else error ("Expected " ++ show leftLength ++ " type parameters, but received " ++ show rightLength)

subsumes a _ | a == auto = error "auto on left hand"
subsumes _ b | b == auto = True
-- TODO find a solution for passing arrays to functions expecting a pointer
subsumes (ArrayType elementType _) (PointerType pointerType) =
    subsumes elementType pointerType
subsumes a b = a == b

find name env = case lookup name env of
    Nothing -> fail ("Cannot find name " ++ show name ++ " in env " ++ show env)
    Just found -> return found
