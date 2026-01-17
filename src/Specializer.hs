{-# LANGUAGE OverloadedStrings #-}
module Specializer(specializeModule) where

import Types
import Control.Monad.Trans.Reader(ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class(lift)
import Data.IORef(IORef, modifyIORef', newIORef, readIORef)
import Platte(transformBiM)
import Resolver(substitute, zipTypeParameters)
import Data.List(partition, nub)
import Data.Text(Text)
import Helpers(overwriteText)


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

type TemplateName = (Text, [Type])

specializeModule :: Module -> IO Module
specializeModule (Module name defs) =
    let
        (permanents, changing) = partition isPermanent defs
        (genericDefs, concreteDefs) = partition hasTypeParameters changing
    in do
        specialized <- specializeDefinitions [] genericDefs concreteDefs
        -- TODO this needs proper dependency sorting
        -- For now, structs come before functions and other definitions
        -- so that they can refer to them in their types
        let (structs, others) = partition isStructDefinition specialized
        return (Module name (permanents ++ structs ++ others))

specializeDefinitions :: [TemplateName] -> [Statement] -> [Statement] -> IO [Statement]
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
            return (specializedDefs ++ modifiedStatements)

specialize :: [Statement] -> ReaderT (IORef [TemplateName]) IO [Statement]
specialize m =
    let
        f e@(Variable _ []) =
            return e
        f e@(Variable name _) | isSpecial (getInnerText name) =
            return e
        f (Variable name typeParameters) = do
            let concreteName = concretize (getInnerText name) typeParameters
            let ty = getType name
            ensureInstance (getInnerText name) typeParameters
            lift (putStrLn ("Ensure specialization " ++ show concreteName ++ " " ++ show (name, typeParameters)))
            return (Variable (Name (overwriteText concreteName (getLocatedText name)) ty) [])
        f e = return e

        g ty@(Concrete _ []) =
            return ty
        g ty@(Concrete name _) | isSpecial (getText name) =
            return ty
        g (Concrete name typeParameters) = do
            let concreteName = concretize (getText name) typeParameters
            ensureInstance (getText name) typeParameters
            lift (putStrLn ("Ensure type specialization " ++ show concreteName ++ " " ++ show (name, typeParameters)))
            return (Concrete (overwriteText concreteName name) [])
        g ty = return ty
    in do
        m' <- transformBiM g m
        transformBiM f m'

filterRequired :: (Foldable t, Eq a) => [a] -> t a -> [a]
filterRequired xs ys = filter (\x -> notElem x ys) xs

generateDefinitions :: [Statement] -> [TemplateName] -> ([TemplateName], [Statement])
generateDefinitions genericDefs required =
    unzip (fmap (uncurry (generateDefinition genericDefs)) required)

generateDefinition :: [Statement] -> Text -> [Type] -> ((Text, [Type]), Statement)
generateDefinition genericDefs genericName typeParameters =
    case lookupDefinition genericName genericDefs of
        Nothing -> error ("Missing generic definition " ++ show genericName)
        Just def -> ((genericName, typeParameters), instantiate genericName typeParameters def)

ensureInstance :: Text -> [Type] -> ReaderT (IORef [TemplateName]) IO ()
ensureInstance name typeParameters = do
    ref <- ask
    lift (modifyIORef' ref (\e -> (name, typeParameters):e))

instantiate :: Text -> [Type] -> Statement -> Statement
instantiate genericName typeParameters (StructDefinition def functionTypeParameters fields) =
    let
        concreteName = concretize genericName typeParameters
        subst = zipTypeParameters (fmap getText functionTypeParameters) typeParameters
        fields' = substitute subst fields
    in StructDefinition (overwriteText concreteName def) [] fields'
instantiate genericName typeParameters (FunctionDefinition def functionTypeParameters returnType parameters body) =
    let
        concreteName = concretize genericName typeParameters
        subst = zipTypeParameters (fmap getText functionTypeParameters) typeParameters
        returnType' = substitute subst returnType
        parameters' = substitute subst parameters
        body' = substitute subst body
    in FunctionDefinition (overwriteText concreteName def) [] returnType' parameters' body'
instantiate _ _ statement = error ("Expected definition for instantiate, but was " ++ show statement)

hasTypeParameters :: Statement -> Bool
hasTypeParameters (FunctionDefinition _ (_:_) _ _ _) = True
hasTypeParameters (StructDefinition _ (_:_) _) = True
hasTypeParameters _ = False

isPermanent :: Statement -> Bool
isPermanent (Import _ _) = True
isPermanent (ExternDefinition _ _ _) = True
isPermanent _ = False

isStructDefinition :: Statement -> Bool
isStructDefinition (StructDefinition _ _ _) = True
isStructDefinition _ = False

lookupDefinition :: Text -> [Statement] -> Maybe Statement
lookupDefinition _ [] = Nothing
lookupDefinition name (def:rest) =
    if getName def == name then Just def else lookupDefinition name rest

getName :: Statement -> Text
getName (FunctionDefinition name _ _ _ _) = getText name
getName (StructDefinition name _ _) = getText name
getName statement = error ("Expected definition for getName, but was " ++ show statement)

-- These receive type parameters, but cannot be specialized, because they have no definition
isSpecial :: Text -> Bool
isSpecial name = elem name ["sizeof", "cast", "Pointer"]

concretize :: Text -> [Type] -> Text
concretize name typeParameters =
    foldl1 (\t1 t2 -> t1 <> "__" <> t2) (name:fmap compactName typeParameters)

compactName :: Type -> Text
compactName (Concrete name []) = getText name
compactName (PointerType ty) = compactName ty <> "ptr"
compactName ty = error ("Failed pretty " ++ show ty)
