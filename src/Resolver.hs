{-# LANGUAGE OverloadedStrings #-}
module Resolver where

import Types
import Control.Monad.Trans.Reader(runReaderT, ask, local, ReaderT)
import Control.Monad(zipWithM)
import Data.Text(Text)
import qualified Data.Text as Text


-- TODO instead of Type we might need to add a scheme
-- type Env = [(Text, ([TypeParameter], Type))]
type Env = [(Text, Type)]

-- comparisons are a bit special. Should we allow comparing non matching types
-- for example `1 == 1.0`?
baseEnv =
    [
        ("==", FunctionType boolType [auto, auto]),
        ("!=", FunctionType boolType [auto, auto]),
        ("nullptr", Concrete "null" [])
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
    in do
        resolved <- with annotations (resolveDefinitions statements)
        return (Module name resolved)


resolveDefinitions = foldr (resolveDefinition resolveTop) (return mempty)

resolveTop (FunctionDefintion text typeParameters returnType parameters body) = do
    resolved <- with (fmap nameToEnv parameters) (resolveStatements body)
    return (FunctionDefintion text typeParameters returnType parameters resolved)
resolveTop d@(ExternDefintion _ _ _) = return d
resolveTop d@(StructDefinition _ _ _) = return d
resolveTop i@(Import _ _) = return i
resolveTop statement =
    fail ("Cannot resolve show " ++ show statement)

nameToEnv (Name n t) = (n, t)

getEnv = ask

with e = local (\env -> e <> env)

resolveStatements = foldr (resolveDefinition resolveStatement) (return mempty)

resolveDefinition _ (Definition (Name name annotatedType) value) rest = do
    resolved <- resolveExpression value annotatedType
    let newType = readType resolved
    rest' <- with (singleton name newType) rest
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
    resolvedStatements <- with (singleton name newType) (resolveStatements statements)
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


singleton text value = [(text, value)]


gatherAnnotations = foldMap gatherAnnotation

-- TODO type schemes
gatherAnnotation (FunctionDefintion text typeParameters returnType parameters _) =
    singleton text (makeFunctionType returnType parameters)
gatherAnnotation (StructDefinition text typeParameters parameters) =
    singleton text (makeFunctionType (Concrete text (fmap (\x -> Concrete x []) typeParameters)) parameters)
gatherAnnotation (ExternDefintion text returnType parameters) =
    singleton text (makeFunctionType returnType parameters)
gatherAnnotation _ = mempty

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

resolveExpression :: Expression -> Type -> ReaderT Env IO Expression
resolveExpression (Variable (Name name _) typeParameters) expectedType = do
    env <- getEnv
    scheme <- find name env
    instantiated <- instantiate name scheme typeParameters
    return (Variable instantiated [])
resolveExpression (DotAccess expression (Name name _) typeParameters) expectedType = do
    resolved <- resolveExpression expression auto
    env <- getEnv
    scheme <- find name env
    instantiated <- instantiate name scheme typeParameters
    return (DotAccess resolved instantiated [])
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

instantiate name ty@(FunctionType _ _) [] = return (Name name ty)
instantiate name ty@(Concrete _ _) [] = return (Name name ty)
instantiate name ty@(ArrayType _ _) [] = return (Name name ty)
instantiate name scheme typeParameters = fail ("Cannot instantiate type " ++ show scheme ++ " with type parameters " ++ show typeParameters)


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
