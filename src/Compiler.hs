{-# LANGUAGE OverloadedStrings #-}
module Compiler where

import Types
import Control.Monad((>=>))
import Control.Monad.Trans.Reader(runReaderT, asks)
import Control.Monad.Trans.Class(lift)
import Data.IORef
import Platte
import Data.Text(Text, pack, unpack, replace)
import Data.Maybe(fromMaybe)
import Drucker


data Env = Env
    -- Name supply
    (IORef Int)
    -- Global data for string literals
    (IORef [Statement])
    -- Type lookup for functions
    (IORef [(Text, Type)])
    -- Type parameter applications
    (IORef [(Text, [Type])])

insert key value env = (key, value) : env

newUnique = do
    ref <- asks (\(Env supply _ _ _) -> supply)
    lift (modifyIORef' ref succ >> readIORef ref)

addToEnv name ty = do
    ref <- asks (\(Env _ _ tyEnv _) -> tyEnv)
    lift (modifyIORef' ref (insert name ty))

ensureInstance name typeParameters = do
    ref <- asks (\(Env _ _ _ typeParameterApplications) -> typeParameterApplications)
    lift (modifyIORef' ref (insert name typeParameters))

findTypeParameterApplications name = do
    ref <- asks (\(Env _ _ _ typeParameterApplications) -> typeParameterApplications)
    env <- lift (readIORef ref)
    return [found | (n, found) <- env, n == name]

-- Transformations
runTransformations m = do
    i <- newIORef 0
    env <- newIORef []
    tyEnv <- newIORef []
    typeParameterApplications <- newIORef []

    result <- runReaderT (transformations m) (Env i env tyEnv typeParameterApplications)
    statements <- readIORef env

    return (statements ++ result)

transformations = toM normalizeTypeNames
    >=> toM makeFunctionsGlobal
    >=> genericVariableIntoSpecialization
    >=> genericDefinitionIntoSpecialization

    >=> genericVariableIntoSpecialization
    >=> genericDefinitionIntoSpecialization

    >=> makeStringsGlobal
    >=> transformIfIntoJumps
    >=> transformWhileIntoJumps
    >=> transformCallIntoDef
    >=> ssa
    >=> autoIntoType
    >=> toM pointersIntoType

toM f x = return (f x)


normalizeTypeNames m =
    let
        f (TypeVariable "int" []) = TypeVariable "i32" []
        f (TypeVariable "long" []) = TypeVariable "i64" []
        f (TypeVariable "uint" []) = TypeVariable "u32" []
        f (TypeVariable "ulong" []) = TypeVariable "u64" []
        f (TypeVariable "bool" []) = TypeVariable "i32" []
        f x = x
    in transformBi f m

-- Create a new variable, put the string into global definitions and swap
makeStringsGlobal m =
    let
        f s@(String _) = do
            n <- fmap (\i -> pack ("s" ++ show i)) newUnique
            ref <- asks (\(Env _ env _ _) -> env)
            lift (modifyIORef' ref (\e -> Definition n (TypeVariable "char" []) s : e))
            return (Variable Global n [])
        f x = return x
    in transformBiM f m

-- TODO proper scoping to enable calling local functions
makeFunctionsGlobal m =
    let
        f (Apply ty (Variable Local name tyVars) params) =
            Apply ty (Variable Global name tyVars) params
        f x = x
    in transformBi f m

-- Qbe documentation states:
-- "Unless the called function does not return a value, a return temporary must be specified, even if it is never used afterwards."
-- Therefore, every call is turned into a definition
transformCallIntoDef m =
    let
        f (Call e) = do
            v <- fmap (\i -> pack ("v" ++ show i)) newUnique
            return (Definition v auto e)
        f x = return x
    in transformBiM f m

{-
f cannot work on expressions directly, because we have to emit definitions
at the correct place

Statements with expressions:

Definition Text Type Expression
Return (Maybe Expression)


Loops are turned into Definition and Labels before:

Call Expression
If [(Expression, [Statement])] (Maybe [Statement])
While Expression [Statement]
-- TODO
For Text Type Expression [Statement]
Switch Expression [(Expression, [Statement])]

TODO Expressions:

DotAccess _ _ _
SquareAccess Expression Expression
ArrayExpression [Expression]
-}
ssa m =
    let
        f (Definition v t (Apply ty e es) : statements) = do
            (e', defs1) <- expressionIntoVariable e
            pairs <- traverse expressionIntoVariable es
            let (es', defs2) = unzip pairs
            return (case defs1 ++ concat defs2 of
                [] -> Nothing
                defs -> Just (defs ++ Definition v t (Apply ty e' es') : statements))
        f (Return (Just e) : statements) = do
            (e', defs) <- expressionIntoVariable e
            return (case defs of
                [] -> Nothing
                _ -> Just (defs ++ Return (Just e') : statements))
        f _ = return Nothing
    in rewriteBiM f m

expressionIntoVariable v@(Variable _ _ _) = return (v, [])
expressionIntoVariable l@(Int64 _) = return (l, [])
expressionIntoVariable l@(Boolean _) = return (l, [])
expressionIntoVariable l@(Float64 _) = return (l, [])
expressionIntoVariable (String _) = error "Strings should no longer exist at this stage"
expressionIntoVariable e = do
    v <- fmap (\i -> pack ("v" ++ show i)) newUnique
    let def = Definition v auto e
    return (Variable Local v [], [def])

{-
Turn if into jumps and labels
    if cond sts1 sts2

    |
    |
    v

    %cond = cond
    jnz %cond, @then, @else
    @then
        sts1
    @else
        sts2
-}
transformIfIntoJumps m =
    let
        f (If [(cond, thenBranch)] elseBranch : statements) = do
            condVar <- fmap (\i -> pack ("cond" ++ show i)) newUnique
            thenVar <- fmap (\i -> pack ("then" ++ show i)) newUnique
            elseVar <- fmap (\i -> pack ("else" ++ show i)) newUnique

            return ([Definition condVar (TypeVariable "i32" []) cond,
                JumpNonZero condVar thenVar elseVar,
                Label thenVar] ++ thenBranch ++ [Label elseVar] ++ concat elseBranch ++ statements)
        f x = return x
    in transformBiM f m

-- TODO lazy comparison operator into jumps
-- This is more advanced, because it probably cannot be done without phi

{-
Turn while into jumps and labels
    if cond sts1 sts2

    |
    |
    v

TODO Which option is better?
Option 2

    @continue
        %cond = cond
        jnz %cond, @loop, @break
    @loop
        sts1
        jmp @continue
    @break
        sts2

Option 1

    jmp @continue
    @loop
        sts1
    @continue
        %cond = cond
        jnz %cond, @loop, @break
    @break
        sts2
-}
transformWhileIntoJumps m =
    let
        f (While cond loopStats : statements) = do
            condVar <- fmap (\i -> pack ("cond" ++ show i)) newUnique
            continueVar <- fmap (\i -> pack ("continue" ++ show i)) newUnique
            loopVar <- fmap (\i -> pack ("loop" ++ show i)) newUnique
            breakVar <- fmap (\i -> pack ("break" ++ show i)) newUnique

            return ([Label continueVar,
                Definition condVar (TypeVariable "i32" []) cond,
                JumpNonZero condVar loopVar breakVar,
                Label loopVar] ++ loopStats ++ [Jump continueVar, Label breakVar] ++ statements)
        f x = return x
    in transformBiM f m

-- IDEA for generic
-- * grab all generic function and struct definitions
-- * go into all function definitions without type parameters (leafs)
-- * look for calls of generic functions and uses of generic structs
-- * instantiate their specialization
--    * substitute all T1 ... Tn with the type parameters    
-- * exchange generic calls with calls to the specialization
-- * for example alloc<int>() is transformed into alloc__int() and specialization alloc__int is created
-- * exchange use of generic structs with use of the specialization
-- * repeat for the specializations, because they also contain generic calls
-- * some generic calls have to be built-in. For example sizeof<int>() or cast<int>(v)

genericVariableIntoSpecialization m =
    let
        f fd@(FunctionDefintion _ _ (_:_) _ _) = return fd
        f s = genericVariableIntoSpecialization' s
    in traverse f m

genericVariableIntoSpecialization' m =
    let
        f v@(Variable _ _ []) =
            return v
        f v@(Variable _ "sizeof" _) =
            return v
        f (Apply _ (Variable Global "sizeof" [t]) []) =
            return (Int64 (calculateSize t))
        f (Variable Global name typeParameters) = do
            _ <- ensureInstance name typeParameters
            let concreteName = concretize name typeParameters
            lift (putStrLn ("Ensure specialization " ++ show concreteName ++ " " ++ show (name, typeParameters)))
            return (Variable Global concreteName [])
        f x = return x
    in transformBiM f m

calculateSize (TypeVariable "i32" []) = 4

genericDefinitionIntoSpecialization m =
    let
        f statements@(FunctionDefintion _ _ [] _ _:_) =
            return statements
        f (FunctionDefintion name returnType functionTypeParameters params body:statements) = do
            typeParameterApplications <- findTypeParameterApplications name
            lift (putStrLn ("Generating specialization for " ++ show (name, typeParameterApplications)))
            let concretes = fmap (instantiate name returnType functionTypeParameters params body) typeParameterApplications
            return (concretes ++ statements)
        f s = return s
    in transformBiM f m

concretize name typeParameters =
    foldl1 (\t1 t2 -> t1 <> "__" <> t2) (name:fmap prettyType typeParameters)

prettyType (TypeVariable n []) = n
prettyType (TypeVariable "UnsafePointer" [t]) = prettyType t <> "ptr"
prettyType (TypeVariable "UnsafeMutablePointer" [t]) = prettyType t <> "mutptr"
prettyType t = error ("Failed pretty " ++ show t)

instantiate name returnType functionTypeParameters params stats typeParameters =
    let
        subst = bind functionTypeParameters typeParameters
        
        concreteName = concretize name typeParameters
        returnType' = substitute subst returnType
        params' = substitute subst params
        stats' = substitute subst stats
    in FunctionDefintion concreteName returnType' [] params' stats'

bind = zip

substitute substitution m =
    let
        f t@(TypeVariable v []) =
            fromMaybe t (lookup v substitution)
        f t = t
    in transformBi f m

-- Turns auto into concrete types
autoIntoType m =
    let
        f (Definition name returnType (Apply _ e@(Variable _ v []) es)) = do
            ref <- asks (\(Env _ _ tyEnv _) -> tyEnv)
            env <- lift (readIORef ref)
            if v == "!=" || v == "=="
                -- TODO operators: get type from paramters and pick correct call ceqw, ceql etc.
                then return (Definition name (TypeVariable "i32" []) (Apply auto e es))
                else (case lookup v env of
                    -- TODO match returnType with (last ts)
                    Just t@(TypeVariable "Fn" ts) -> return (Definition name (last ts) (Apply t e es))
                    Nothing -> fail ("Unknown " ++ show v ++ " in " ++ show (fmap fst env))
                    Just t -> fail ("Non function type " ++ show t ++ " for " ++ show v ++ " in " ++ show (fmap fst env)))
        f (FunctionDefintion name returnType [] parameters statements) = do
            addToEnv name (TypeVariable "Fn" (fmap snd parameters ++ [returnType]))
            statements' <- traverse f statements
            return (FunctionDefintion name returnType [] parameters statements')
        f s@(ExternDefintion name returnType parameters) = do
            addToEnv name (TypeVariable "Fn" (fmap snd parameters ++ [returnType]))
            return s
        f s = return s
    in traverse f m

pointersIntoType m =
    let
        f (TypeVariable "UnsafeMutablePointer" _) = TypeVariable "ptr" []
        f (TypeVariable "UnsafePointer" _) = TypeVariable "ptr" []
        f t = t
    in transformBi f m

-- Pretty Printing to Qbe format
toQbe s = intercalate "\n\n" (fmap toQbeP s)

-- Top level
toQbeP (Definition name ty e) =
    "data" <+> "$" <> fromText name <+> "=" <+> "{" <+> toQbeT ty <+> toQbeE e <+> "}"
toQbeP s = toQbeS s

-- TODO use type in comparisons for float and similar
toQbeS (Definition name ty (Apply t (Variable _ "==" _) [e1, e2])) =
    indent ("%" <> fromText name <+> "=" <> toQbeT ty <+> "ceql" <+> toQbeE e1 <> "," <+> toQbeE e2)
toQbeS (Definition name ty (Apply t (Variable _ "!=" _) [e1, e2])) =
    indent ("%" <> fromText name <+> "=" <> toQbeT ty <+> "cnel" <+> toQbeE e1 <> "," <+> toQbeE e2)
-- No variable necessary for void
toQbeS (Definition _ (TypeVariable "void" []) (Apply ty v parameters)) =
    indent (toQbeCall ty v parameters)
toQbeS (Definition name returnType (Apply ty v parameters)) =
    indent ("%" <> fromText name <+> "=" <> toQbeT returnType <+> toQbeCall ty v parameters)
toQbeS (Definition name ty e) =
    indent ("%" <> fromText name <+> "=" <> toQbeT ty <+> toQbeE e)
toQbeS (FunctionDefintion name ty _ parameters statements) =
    "export function" <+> toQbeT ty <+> "$" <> fromText name <> "(" <> intercalate ", " (fmap toQbeFunParam parameters) <> ")" <+> "{"
    <//> "@start"
    <//> intercalate "\n" (fmap toQbeS statements)
    <//> "}"
toQbeS (Return (Just e)) = indent ("ret" <+> toQbeE e)
toQbeS (Return Nothing) = indent "ret"
toQbeS (JumpNonZero c t e) = indent ("jnz" <+> "%" <> fromText c <> "," <+> "@" <> fromText t <> "," <+> "@" <> fromText e)
toQbeS (Jump l) = indent ("jmp" <+> "@" <> fromText l)
toQbeS (Label e) = "@" <> fromText e
-- TODO filter these out, because they create a lot of whitespace
toQbeS (Import _ _) = ""
toQbeS (ExternDefintion _ _ _) = ""
toQbeS (StructDefinition name _ parameters) =
    "type" <+> ":" <> fromText name <+> "=" <+> "{"
        <> intercalate ", " (fmap toQbeStructParam parameters)
        <> "}"
toQbeS other = error ("Error: QbeS Following statement appearedd in printing stage " ++ show other)

toQbeCall (TypeVariable "Fn" tys) v parameters =
    let
        parametersWithType = zip parameters tys
    in "call" <+> toQbeE v <> "(" <> intercalate ", " (fmap toQbeParam parametersWithType) <> ")"
toQbeCall t v _ = error ("Error: Non-function type in toQbeCall " ++ show t ++ " calling " ++ show v)

toQbeFunParam (name, ty) = toQbeT ty <+> "%" <> fromText name

toQbeParam (e, ty) = toQbeT ty <+> toQbeE e

-- TODO add size for arrays
toQbeStructParam (name, ty) = toQbeT ty

toQbeE (Variable Local name _) = "%" <> fromText name
toQbeE (Variable Global name _) = "$" <> fromText name
toQbeE (Int64 l) = fromText (pack (show l))
toQbeE (Float64 l) = fromText (pack (show l))
toQbeE (Boolean True) = "true"
toQbeE (Boolean False) = "false"
toQbeE (String s) = "\"" <> fromText (escape s) <> "\""
toQbeE other = error ("QbeE " ++ show other)

-- TODO check if \0 escpae character works in QBE
escape = replace "\0" "\\0"
    . replace "\n" "\\n"
    . replace "\t" "\\t"
    . replace "\r" "\\r"
    . replace "\"" "\\\""
    . replace "\'" "\\\'"
    . replace "\\" "\\\\"

-- TODO Accessing a struct is just a read at offset
--toQbeE (DotAccess e name typeParameters) =
-- TODO Accessing an array is just a read at offset
--toQbeE (SquareAccess e1 e2) =
-- TODO Creating an array is just allocating on stack and filling it
--toQbeE (ArrayExpression es) =

-- TODO decide between string pointer and string
toQbeT (TypeVariable "char" []) = "b"
toQbeT (TypeVariable "void" []) = " "
toQbeT (TypeVariable "i32" []) = "w"
toQbeT (TypeVariable "i64" []) = "l"
toQbeT (TypeVariable "ptr" []) = "l"
toQbeT (TypeVariable "auto" []) = error "Error: auto appeared in printing stage"
toQbeT (TypeVariable s []) = ":" <> fromText s
toQbeT (TypeVariable s typeParameters) =
    error ("Error: generic type "
        ++ unpack s ++ show typeParameters ++ " appeared in printing stage")



toCs s = intercalate "\n\n" (fmap toCsS s)

-- Top level
toCsS (Definition name ty e) =
    "const" <+> toCsT ty <+> fromText name <+> "=" <+> toCsE e <> ";"
toCsS (Call e) = toCsE e <> ";"
toCsS (FunctionDefintion name ty typeParameters parameters statements) =
    "public static" <+> toCsT ty <+> fromText name <> toCsTypDefParams typeParameters <> "(" <> intercalate ", " (fmap toCsFunParam parameters) <> ")"
        <//> "{"
        <//> indent (intercalate "\n" (fmap toCsS statements))
        <//> "}"
toCsS (Return (Just e)) = "return" <+> toCsE e <> ";"
toCsS (Return Nothing) = "return;"
toCsS (Import _ _) = ""
toCsS (ExternDefintion _ _ _) = ""
toCsS (StructDefinition name _ parameters) =
    "public struct" <+> fromText name
        <//> "{"
        <//> indent (intercalate "\n" (fmap toCsStructParam parameters))
        <//> "}"
toCsS (If conds Nothing) =
    intercalate "else" (fmap printIfPart conds)
toCsS (If conds (Just th)) =
    intercalate "else" (fmap printIfPart conds ++ [printElsePart th])
toCsS (While cond sts) = "while" <+> "(" <> toCsE cond <> ")" 
    <//> "{"
    <//> indent (intercalate "\n" (fmap toCsS sts))
    <//> "}"
toCsS other = error ("Error: CsS Following statement appearedd in printing stage " ++ show other)

printIfPart (cond, sts) = "if" <+> "(" <> toCsE cond <> ")"
    <//> "{"
    <//> indent (intercalate "\n" (fmap toCsS sts))
    <//> "}"

printElsePart sts = "{"
    <//> indent (intercalate "\n" (fmap toCsS sts))
    <//> "}"

toCsFunParam (name, ty) = toCsT ty <+> fromText name

toCsTypDefParams [] = mempty
toCsTypDefParams tyNames = "<" <> intercalate ", " (fmap fromText tyNames) <> ">"

toCsTypParams [] = mempty
toCsTypParams typeParameters = "<" <> intercalate ", " (fmap toCsT typeParameters) <> ">"

toCsParam (e, ty) = toCsT ty <+> toCsE e

toCsStructParam (name, ty) = "public" <+> toCsT ty <+> fromText name

toCsE (Variable _ name typeParameters) = fromText name <> toCsTypParams typeParameters
toCsE (Int64 l) = fromText (pack (show l))
toCsE (Float64 l) = fromText (pack (show l))
toCsE (Boolean True) = "true"
toCsE (Boolean False) = "false"
toCsE (String s) = "\"" <> fromText (escape s) <> "\""
toCsE (Apply _ (Variable _ "==" _) [e1, e2]) =
    toCsE e1 <+> "==" <+> toCsE e2
toCsE (Apply _ (Variable _ "!=" _) [e1, e2]) =
    toCsE e1 <+> "!=" <+> toCsE e2
toCsE (Apply _ e es) = toCsE e <> "(" <> intercalate ", " (fmap toCsE es) <> ")"
toCsE (DotAccess e name typeParameters) =
    toCsE e <> "." <> fromText name <> toCsTypParams typeParameters
toCsE (SquareAccess e1 e2) =
    toCsE e1 <> "[" <> toCsE e2 <> "]"
toCsE (ArrayExpression es) =
    "[" <> intercalate ", " (fmap toCsE es) <> "]"
toCsE other = error ("CsE " ++ show other)

toCsT (TypeVariable s typeParameters) =
    fromText s <> toCsTypParams typeParameters
