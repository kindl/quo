{-# LANGUAGE OverloadedStrings #-}
module Resolver where

import Types
import Control.Monad.Trans.Reader(runReaderT, asks, local, ReaderT)
import Control.Monad(zipWithM)
import Data.Text(Text)
import qualified Data.Text as Text
import Platte
import Data.Maybe(fromMaybe)


data Env = Env [(Text, ([TypeParameter], Type))] [(Text, ([TypeParameter], [(Text, Type)]))]

-- comparisons are a bit special. Should we allow comparing non matching types
-- for example `1 == 1.0`?
baseEnv :: Env
baseEnv =
    Env [
        ("==", ([], FunctionType boolType [auto, auto])),
        ("!=", ([], FunctionType boolType [auto, auto])),
        ("nullptr", ([], Concrete "null" []))
    ] [
        ("Pointer", (["T"], [("value", Concrete "T" [])]))
    ]

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

-- TODO check return type or pass return type
resolveTop (FunctionDefintion text typeParameters returnType parameters body) = do
    resolved <- with (fmap nameToEntry parameters) (resolveStatements body)
    return (FunctionDefintion text typeParameters returnType parameters resolved)
resolveTop d@(ExternDefintion _ _ _) = return d
resolveTop d@(StructDefinition _ _ _) = return d
resolveTop i@(Import _ _) = return i
resolveTop statement =
    fail ("Cannot resolve show " ++ show statement)


getVariableEnv = asks (\(Env e _) -> e)

getStructEnv = asks (\(Env _ e) -> e)

with env = local (\(Env variableEnv structEnv) -> Env (variableEnv <> env) structEnv)

with' e s = local (\(Env variableEnv structEnv) -> Env (variableEnv <> e) (structEnv <> s))


resolveStatements = foldr (resolveDefinition resolveStatement) (return mempty)

resolveDefinition _ (Definition (Name name annotatedType) value) rest = do
    resolved <- resolveExpression value annotatedType
    let newType = readType resolved
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

nameToEntry (Name n t) = (n, ([], t))

entry text value = entry' text [] value

entry' text typeParameters value = [(text, (typeParameters, value))]


gatherAnnotations = foldMap gatherAnnotation

gatherAnnotation (FunctionDefintion text typeParameters returnType parameters _) =
    entry' text typeParameters (makeFunctionType returnType parameters)
gatherAnnotation (StructDefinition text typeParameters parameters) =
    entry' text typeParameters (makeFunctionType (Concrete text (fmap (\x -> Concrete x []) typeParameters)) parameters)
gatherAnnotation (ExternDefintion text returnType parameters) =
    entry text (makeFunctionType returnType parameters)
gatherAnnotation _ = mempty

gatherStructs = foldMap gatherStruct

gatherStruct (StructDefinition text typeParameters parameters) =
    [(text, (typeParameters, fmap nameToPair parameters))]
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
readType (ArrayExpression []) = error "TODO empty array types"
readType (ArrayExpression expressions) =
    let
        ty = readType (head expressions)
    in ArrayType ty (Just (fromIntegral (length expressions)))

literalType (StringLiteral l) = ArrayType (Concrete "char" []) (Just (fromIntegral (Text.length l + 1)))
literalType (Bool _) = boolType
literalType (Int32 _) = intType
literalType (Int64 _) = Concrete "long" []
literalType (Float32 _) = Concrete "float" []
literalType (Float64 _) = Concrete "double" []

-- TODO when are struct types instantiated?
resolveExpression :: Expression -> Type -> ReaderT Env IO Expression
resolveExpression (Variable (Name name _) typeParameters) expectedType = do
    env <- getVariableEnv
    (typeVars, scheme) <- find name env
    let ty = instantiate scheme typeVars typeParameters
    return (Variable (Name name ty) typeParameters)
-- TODO when do these type parameters come into effect?
-- when using namespace access? someModuleName.function<...>()
-- can structs have methods, and if yes, can they have type parameters? someStruct.function<...>()
resolveExpression (DotAccess expression (Name name _) typeParameters) expectedType = do
    resolved <- resolveExpression expression auto
    let (Concrete structName structTypeParameters) = readType resolved
    env <- getStructEnv
    (typeVars, fields) <- find structName env
    let instantiated = instantiate fields typeVars structTypeParameters
    fieldType <- find name instantiated
    return (DotAccess resolved (Name name fieldType) typeParameters)
resolveExpression (Apply expression parameters) expectedType = do
    resolvedFunction <- resolveExpression expression auto
    let functionType = readType resolvedFunction
    case functionType of
        FunctionType returnType _ | not (subsumes returnType expectedType) -> fail "TODO return type"
        FunctionType _ parameterTypes ->
            fmap (Apply resolvedFunction) (zipWithM resolveExpression parameters parameterTypes)
        other -> fail ("Cannot apply expression " ++ show resolvedFunction ++ " without function type " ++ show other)
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

instantiate ty [] [] = ty
instantiate ty vars vals = substitute (zip vars vals) ty

substitute substitution m =
    let
        f t@(Concrete v []) =
            fromMaybe t (lookup v substitution)
        f t = t
    in transformBi f m

safeZip f xs ys = if length xs /= length ys then fail "TODO safeZip" else zipWithM f xs ys

subsumes a _ | a == auto = error "auto on left hand"
subsumes _ b | b == auto = True
-- TODO find a solution for passing arrays to functions expecting a pointer
subsumes (ArrayType elementType _) (Concrete "UnsafePointer" [pointerType]) =
    subsumes elementType pointerType
subsumes a b = a == b

find name env = case lookup name env of
    Nothing -> fail ("Cannot find name " ++ show name ++ " in env " ++ show env)
    Just found -> return found
