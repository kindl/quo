{-# LANGUAGE OverloadedStrings #-}
module Specializer where

import Types
import Control.Monad.Trans.Reader(runReaderT, ask)
import Control.Monad.Trans.Class(lift)
import Data.IORef
import Platte
import Resolver
import Data.List(partition, nub)


-- * grab all generic function and struct definitions
-- * go into all function definitions without type parameters, as well as top level definitions (leafs)
-- * look for calls of generic functions and uses of generic structs
-- * instantiate their specialization
--    * substitute all T1 ... Tn with the type parameters
-- * exchange generic calls with calls to the specialization
-- * for example alloc<int>() is transformed into alloc__int() and specialization alloc__int is created
-- * exchange use of generic structs with use of the specialization
-- * repeat for the specializations, because they also contain generic calls
-- * some generic calls have to be built-in. For example sizeof<int>() or cast<int>(v)

-- * idea for specialization:
-- * because a call like alloc<int>() is just transformed into alloc__int(),
--     we could allow users to just provide an alloc__int() themselves and check if the parameters and return type match

specializeModule (Module name defs) =
    let
        (genericDefs, concreteDefs) = partition hasTypeParameters defs
    in do
        specialized <- specializeDefinitions [] genericDefs concreteDefs
        return (Module name specialized)

specializeDefinitions alreadyGenerated genericDefs concreteDefs = do
    putStrLn ("Running specialize definitions. " ++ show alreadyGenerated ++ " " ++ show (length genericDefs) ++ " " ++ show (length concreteDefs))
    referencedRef <- newIORef []
    -- This returns the modified statements and the specializations that have been referenced
    modifiedStatements <- runReaderT (specialize concreteDefs) referencedRef
    referenced <- readIORef referencedRef
    -- We only want to create definitions that have not been generated
    case filterRequired (nub referenced) alreadyGenerated of
        [] -> return modifiedStatements
        required -> do
            putStrLn ("Generating definitions for " ++ show required)
            let (generatedNames, generatedDefs) = generateDefinitions genericDefs required
            -- The generated specializations can also contain generic calls and have to specialized
            specializedDefs <- specializeDefinitions (alreadyGenerated ++ generatedNames) genericDefs generatedDefs
            return (modifiedStatements ++ specializedDefs)

specialize m =
    let
        f e@(Variable _ []) =
            return e
        f e@(Variable (Name name _) _) | isSpecial name =
            return e
        f (Variable (Name name ty) typeParameters) = do
            let concreteName = concretize name typeParameters
            ensureInstance name typeParameters
            lift (putStrLn ("Ensure specialization " ++ show concreteName ++ " " ++ show (name, typeParameters)))
            return (Variable (Name concreteName ty) [])
        f e = return e

        g ty@(Concrete _ []) =
            return ty
        g ty@(Concrete name _) | isSpecial name =
            return ty
        g (Concrete name typeParameters) = do
            let concreteName = concretize name typeParameters
            ensureInstance name typeParameters
            lift (putStrLn ("Ensure type specialization " ++ show concreteName ++ " " ++ show (name, typeParameters)))
            return (Concrete concreteName [])
        g ty = return ty
    in do
        m' <- transformBiM g m
        transformBiM f m'

filterRequired xs ys = filter (\x -> notElem x ys) xs

generateDefinitions genericDefs required =
    unzip (fmap (uncurry (generateDefinition genericDefs)) required)

generateDefinition genericDefs genericName typeParameters =
    case lookupDefinition genericName genericDefs of
        Nothing -> error ("Missing generic definition " ++ show genericName)
        Just def -> ((genericName, typeParameters), instantiate genericName typeParameters def)

ensureInstance name typeParameters = do
    ref <- ask
    lift (modifyIORef' ref (\e -> (name, typeParameters):e))

instantiate genericName typeParameters (StructDefinition _ functionTypeParameters fields) =
    let
        concreteName = concretize genericName typeParameters
        subst = zipTypeParameters functionTypeParameters typeParameters
        fields' = substitute subst fields
    in StructDefinition concreteName [] fields'
instantiate genericName typeParameters (FunctionDefintion _ functionTypeParameters returnType parameters body) =
    let
        concreteName = concretize genericName typeParameters
        subst = zipTypeParameters functionTypeParameters typeParameters
        returnType' = substitute subst returnType
        parameters' = substitute subst parameters
        body' = substitute subst body
    in FunctionDefintion concreteName [] returnType' parameters' body'
instantiate _ _ statement = error ("Expected definition for instantiate, but was " ++ show statement)


hasTypeParameters (FunctionDefintion _ (_:_) _ _ _) = True
hasTypeParameters (StructDefinition _ (_:_) _) = True
hasTypeParameters _ = False

lookupDefinition _ [] = Nothing
lookupDefinition name (def:rest) =
    if getName def == name then Just def else lookupDefinition name rest

getName (FunctionDefintion name _ _ _ _) = name
getName (StructDefinition name _ _) = name
getName statement = error ("Expected definition for getName, but was " ++ show statement)

isSpecial name = elem name ["sizeof", "Pointer"]

concretize name typeParameters =
    foldl1 (\t1 t2 -> t1 <> "__" <> t2) (name:fmap compactName typeParameters)

compactName (Concrete name []) = name
compactName (PointerType ty) = compactName ty <> "ptr"
compactName ty = error ("Failed pretty " ++ show ty)
