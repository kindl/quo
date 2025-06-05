{-# LANGUAGE OverloadedStrings #-}
module Resolver where


import Types
import Control.Monad.Trans.Reader(runReaderT, ask, local, ReaderT)
import Data.Text(Text)


type Env = [(Text, Type)]

runResolve m = runReaderT (resolveModule m) []

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

resolveStatements = foldr (resolveDefinition resolveStatement) (return mempty)

resolveDefinition alt (Definition (Name name _) value) rest = do
    resolved <- resolveExpression value
    -- TODO match type
    let newType = readType resolved
    rest' <- with (singleton name newType) rest
    return (Definition (Name name newType) resolved : rest')
resolveDefinition alt statement rest = do
    resolved <- alt statement
    rest' <- rest
    return (resolved:rest')

resolveStatement (Return maybeExpression) =
    fmap Return (traverse resolveExpression maybeExpression)
resolveStatement (Call expression) =
    fmap Call (resolveExpression expression)
resolveStatement (Assignment leftHand rightHand) =
    -- TODO check if left hand and right hand type match
    liftA2 Assignment (resolveExpression leftHand) (resolveExpression rightHand)
resolveStatement (If branches elseBranch) = do
    resolvedOptions <- traverse resolveBranch branches
    resolvedElseBranch <- traverse resolveStatements elseBranch
    return (If resolvedOptions resolvedElseBranch)
resolveStatement (While condition statements) =
    liftA2 While (resolveExpression condition) (resolveStatements statements)
resolveStatement (For (Name name _) expression statements) = do
    resolvedExpression <- resolveExpression expression
    -- TODO match type
    let newType = readType resolvedExpression
    resolvedStatements <- with (singleton name newType) (resolveStatements statements)
    return (For (Name name newType) resolvedExpression resolvedStatements)
resolveStatement (Switch expression branches) = do
    -- TODO match expression type and branch type
    resolvedExpression <- resolveExpression expression
    resolvedBranches <- traverse resolveBranch branches
    return (Switch resolvedExpression resolvedBranches)

resolveBranch (condition, statements) =
    liftA2 (,) (resolveExpression condition) (resolveStatements statements)

with e = local (\env -> e <> env)

-- TODO how to represent generics? They probably need a type scheme and have to be instantiated at use


singleton text value = [(text, value)]


gatherAnnotations = foldMap gatherAnnotation

gatherAnnotation (FunctionDefintion text typeParameters returnType parameters _) =
    singleton text (makeFunctionType typeParameters returnType parameters)
gatherAnnotation (StructDefinition text typeParameters parameters) =
    singleton text (makeFunctionType typeParameters (Concrete text (fmap (\x -> Concrete x []) typeParameters)) parameters)
gatherAnnotation (ExternDefintion text returnType parameters) =
    singleton text (makeFunctionType [] returnType parameters)
gatherAnnotation _ = mempty

-- TODO arrays, square access
readType :: Expression -> Type
readType (Variable (Name _ ty) _) = ty
readType (DotAccess _ (Name _ ty) _) = ty
readType (Apply e _) = case readType e of
    FunctionType _ returnType _ -> returnType
    ty -> error ("Cannot read type of expression " ++ show e ++ " which applies " ++ show ty)
readType (Literal l) = literalType l
readType other = error ("Cannot read type of expression " ++ show other)

literalType (StringLiteral _) = Concrete "string" []
literalType (Bool _) = Concrete "bool" []
literalType (Int32 _) = Concrete "int" []
literalType (Int64 _) = Concrete "long" []
literalType (Float32 _) = Concrete "float" []
literalType (Float64 _) = Concrete "double" []

resolveExpression :: Expression -> ReaderT Env IO Expression
resolveExpression (Variable (Name name _) typeParameters) = do
    env <- ask
    found <- find name env
    -- TODO instantiate if it is a scheme
    return (Variable (Name name found) typeParameters)
resolveExpression (DotAccess expression (Name name _) typeParameters) = do
    resolved <- resolveExpression expression
    env <- ask
    found <- find name env
    -- TODO instantiate if it is a scheme
    return (DotAccess resolved (Name name found) typeParameters)
resolveExpression (Apply expression parameters) =
    liftA2 Apply (resolveExpression expression) (traverse resolveExpression parameters)
resolveExpression (SquareAccess expression access) =
    liftA2 SquareAccess (resolveExpression expression) (resolveExpression access)
resolveExpression (ArrayExpression expressions) =
    fmap ArrayExpression (traverse resolveExpression expressions)
resolveExpression l@(Literal _) = return l


find name env = case lookup name env of
    Nothing -> fail ("Cannot find name " ++ show name ++ " in env " ++ show env)
    Just found -> return found
