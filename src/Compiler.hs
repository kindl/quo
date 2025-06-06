{-# LANGUAGE OverloadedStrings #-}
module Compiler where

import Types
import Control.Monad((>=>))
import Control.Monad.Trans.Reader(runReaderT, asks)
import Control.Monad.Trans.Class(lift)
import Data.IORef
import Platte
import qualified Data.Text as Text
import Data.Text(Text, pack, unpack, replace)
import Data.Maybe(fromMaybe)
import Drucker
import Resolver(readType)


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
runTransformations (Module m statements) = do
    i <- newIORef 0
    env <- newIORef []
    tyEnv <- newIORef []
    typeParameterApplications <- newIORef []

    result <- runReaderT (transformations statements) (Env i env tyEnv typeParameterApplications)
    instances <- readIORef env

    return (Module m (instances ++ result))

transformations =
    genericVariableIntoSpecialization
    >=> genericDefinitionIntoSpecialization

    >=> genericVariableIntoSpecialization
    >=> genericDefinitionIntoSpecialization

    >=> makeStringsGlobal
    -- >=> transformIfIntoJumps
    -- >=> transformWhileIntoJumps
    >=> transformCallIntoDef
    >=> ssa
    >=> autoIntoType

-- Create a new variable, put the string into global definitions and swap
makeStringsGlobal m =
    let
        f s@(Literal (StringLiteral l)) = do
            u <- fmap (\i -> pack ("s" ++ show i)) newUnique
            -- TODO finding correct size, accout for null byte etc
            let name = Name u (ArrayType (Concrete "char" []) (Just (fromIntegral (Text.length l))))
            ref <- asks (\(Env _ env _ _) -> env)
            lift (modifyIORef' ref (\e -> Definition name s : e))
            return (Variable name [])
        f x = return x
    in transformBiM f m

-- Qbe documentation states:
-- "Unless the called function does not return a value, a return temporary must be specified, even if it is never used afterwards."
-- Therefore, every call is turned into a definition
transformCallIntoDef m =
    let
        f (Call e) = do
            v <- fmap (\i -> pack ("v" ++ show i)) newUnique
            return (Definition (Name v auto) e)
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
        f (Definition (Name v t) (Apply e es) : statements) = do
            (e', defs1) <- expressionIntoVariable e
            pairs <- traverse expressionIntoVariable es
            let (es', defs2) = unzip pairs
            return (case defs1 ++ concat defs2 of
                [] -> Nothing
                defs -> Just (defs ++ Definition (Name v t) (Apply e' es') : statements))
        f (Return (Just e) : statements) = do
            (e', defs) <- expressionIntoVariable e
            return (case defs of
                [] -> Nothing
                _ -> Just (defs ++ Return (Just e') : statements))
        f _ = return Nothing
    in rewriteBiM f m

expressionIntoVariable v@(Variable _ _) = return (v, [])
expressionIntoVariable (Literal (StringLiteral _)) = error "Strings should no longer exist at this stage"
expressionIntoVariable l@(Literal _) = return (l, [])
expressionIntoVariable e = do
    v <- fmap (\i -> pack ("v" ++ show i)) newUnique
    let name = Name v auto
    let def = Definition name e
    return (Variable name [], [def])

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
transformIfIntoJumps m =
    let
        f (If [(cond, thenBranch)] elseBranch : statements) = do
            condVar <- fmap (\i -> pack ("cond" ++ show i)) newUnique
            thenVar <- fmap (\i -> pack ("then" ++ show i)) newUnique
            elseVar <- fmap (\i -> pack ("else" ++ show i)) newUnique

            return ([Definition (Name condVar (Concrete "int" [] Nothing)) cond,
                JumpNonZero condVar thenVar elseVar,
                Label thenVar] ++ thenBranch ++ [Label elseVar] ++ concat elseBranch ++ statements)
        f x = return x
    in transformBiM f m
-}

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
transformWhileIntoJumps m =
    let
        f (While cond loopStats : statements) = do
            condVar <- fmap (\i -> pack ("cond" ++ show i)) newUnique
            continueVar <- fmap (\i -> pack ("continue" ++ show i)) newUnique
            loopVar <- fmap (\i -> pack ("loop" ++ show i)) newUnique
            breakVar <- fmap (\i -> pack ("break" ++ show i)) newUnique

            return ([Label continueVar,
                Definition (Name condVar (Concrete "int" [] Nothing)) cond,
                JumpNonZero condVar loopVar breakVar,
                Label loopVar] ++ loopStats ++ [Jump continueVar, Label breakVar] ++ statements)
        f x = return x
    in transformBiM f m
-}

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
        f fd@(FunctionDefintion _ (_:_) _ _ _) = return fd
        f s = genericVariableIntoSpecialization' s
    in traverse f m

genericVariableIntoSpecialization' m =
    let
        f v@(Variable _ []) =
            return v
        f v@(Variable (Name "sizeof" _) _) =
            return v
        f (Apply (Variable (Name "sizeof" _) [t]) []) =
            return (Literal (Int64 (calculateSize t)))
        f (Variable (Name name ty) typeParameters) = do
            _ <- ensureInstance name typeParameters
            let concreteName = concretize name typeParameters
            lift (putStrLn ("Ensure specialization " ++ show concreteName ++ " " ++ show (name, typeParameters)))
            return (Variable (Name concreteName ty) [])
        f x = return x
    in transformBiM f m

-- TODO array sizes
calculateSize (Concrete "int" []) = 4

genericDefinitionIntoSpecialization m =
    let
        f statements@(FunctionDefintion _ [] _ _ _:_) =
            return statements
        f (FunctionDefintion name typeParameters returnType params body:statements) = do
            typeParameterApplications <- findTypeParameterApplications name
            lift (putStrLn ("Generating specialization for function" ++ show (name, typeParameterApplications)))
            let concretes = fmap (instantiate name returnType typeParameters params body) typeParameterApplications
            return (concretes ++ statements)
        f (StructDefinition name typeParameters params:statements) = do
            typeParameterApplications <- findTypeParameterApplications name
            lift (putStrLn ("Generating specialization for struct" ++ show (name, typeParameterApplications)))
            let concretes = fmap (instantiateStruct name typeParameters params) typeParameterApplications
            return (concretes ++ statements)
        f s = return s
    in transformBiM f m

concretize name typeParameters =
    foldl1 (\t1 t2 -> t1 <> "__" <> t2) (name:fmap prettyType typeParameters)

prettyType (Concrete n []) = n
prettyType (Concrete "UnsafePointer" [t]) = prettyType t <> "ptr"
prettyType (Concrete "UnsafeMutablePointer" [t]) = prettyType t <> "mutptr"
prettyType t = error ("Failed pretty " ++ show t)

instantiateStruct name functionTypeParameters params typeParameters =
    let
        subst = bind functionTypeParameters typeParameters

        concreteName = concretize name typeParameters
        params' = substitute subst params
    in StructDefinition concreteName [] params'

instantiate name returnType functionTypeParameters params stats typeParameters =
    let
        subst = bind functionTypeParameters typeParameters

        concreteName = concretize name typeParameters
        returnType' = substitute subst returnType
        params' = substitute subst params
        stats' = substitute subst stats
    in FunctionDefintion concreteName [] returnType' params' stats'

bind = zip

substitute substitution m =
    let
        f t@(Concrete v []) =
            fromMaybe t (lookup v substitution)
        f t = t
    in transformBi f m

-- Turns auto into concrete types
autoIntoType m =
    let
        f (Definition (Name name returnType) (Apply e@(Variable (Name v _) []) es)) = do
            ref <- asks (\(Env _ _ tyEnv _) -> tyEnv)
            env <- lift (readIORef ref)
            if isOperator v
                -- TODO operators: get type from paramters and pick correct call ceqw, ceql etc.
                then return (Definition (Name name (Concrete "int" [])) (Apply e es))
                else (case lookup v env of
                    -- TODO match returnTypes
                    Just (FunctionType callReturnType _) -> return (Definition (Name name callReturnType) (Apply e es))
                    Nothing -> fail ("Unknown " ++ show v ++ " in " ++ show (fmap fst env))
                    Just t -> fail ("Non function type " ++ show t ++ " for " ++ show v ++ " in " ++ show (fmap fst env)))
        -- TODO allow generic functions at this point?
        f (FunctionDefintion name [] returnType parameters statements) = do
            addToEnv name (makeFunctionType returnType parameters)
            -- TODO add parameter types only locally
            traverse (\(Name i t) -> addToEnv i t) parameters
            statements' <- traverse f statements
            return (FunctionDefintion name [] returnType parameters statements')
        f (StructDefinition name [] parameters) = do
            let returnType = Concrete name []
            addToEnv name (makeFunctionType returnType parameters)
            -- TODO add parameter types only locally
            traverse (\(Name i t) -> addToEnv i t) parameters
            return (StructDefinition name [] parameters)
        f s@(ExternDefintion name returnType parameters) = do
            addToEnv name (makeFunctionType returnType parameters)
            return s
        f s = return s
    in traverse f m

-- Pretty Printing to Qbe format
toQbe s = intercalate "\n\n" (fmap toQbeP s)

-- Top level
toQbeP (Definition (Name name ty) e) =
    "data" <+> "$" <> fromText name <+> "=" <+> "{" <+> toQbeT ty <+> toQbeE e <+> "}"
toQbeP s = toQbeS s

-- TODO use type in comparisons for float and similar
toQbeS (Definition (Name name ty) (Apply (Variable (Name "==" _) _) [e1, e2])) =
    indent ("%" <> fromText name <+> "=" <> toQbeT ty <+> "ceql" <+> toQbeE e1 <> "," <+> toQbeE e2)
toQbeS (Definition (Name name ty) (Apply (Variable (Name "!=" _) _) [e1, e2])) =
    indent ("%" <> fromText name <+> "=" <> toQbeT ty <+> "cnel" <+> toQbeE e1 <> "," <+> toQbeE e2)
-- No variable necessary for void
toQbeS (Definition (Name "_" (Concrete "void" [])) (Apply v parameters)) =
    indent (toQbeCall (readType v) v parameters)
toQbeS (Definition (Name name returnType) (Apply v parameters)) =
    indent ("%" <> fromText name <+> "=" <> toQbeT returnType <+> toQbeCall (readType v) v parameters)
toQbeS (Definition (Name name ty) e) =
    indent ("%" <> fromText name <+> "=" <> toQbeT ty <+> toQbeE e)
toQbeS (FunctionDefintion name _ ty parameters statements) =
    "export function" <+> toQbeT ty <+> "$" <> fromText name
    <> parens (intercalate ", " (fmap toQbeFunParam parameters))
    <+> "{"
    <//> "@start"
    <//> intercalate "\n" (fmap toQbeS statements)
    <//> "}"
toQbeS (Return (Just e)) = indent ("ret" <+> toQbeE e)
toQbeS (Return Nothing) = indent "ret"
-- toQbeS (JumpNonZero c t e) = indent ("jnz" <+> "%" <> fromText c <> "," <+> "@" <> fromText t <> "," <+> "@" <> fromText e)
-- toQbeS (Jump l) = indent ("jmp" <+> "@" <> fromText l)
-- toQbeS (Label e) = "@" <> fromText e
-- TODO filter these out, because they create a lot of whitespace
toQbeS (Import _ _) = ""
toQbeS (ExternDefintion _ _ _) = ""
toQbeS (StructDefinition name _ parameters) =
    "type" <+> ":" <> fromText name <+> "=" <+> "{"
        <> intercalate ", " (fmap toQbeStructParam parameters)
        <> "}"
toQbeS other = error ("Error: QbeS Following statement appearedd in printing stage " ++ show other)

toQbeCall (FunctionType _ tys) v parameters =
    let
        parametersWithType = zip parameters tys
    in "call" <+> toQbeE v <> parens (intercalate ", " (fmap toQbeParam parametersWithType))
toQbeCall t v _ = error ("Error: Non-function type in toQbeCall " ++ show t ++ " calling " ++ show v)

toQbeFunParam (Name name ty) = toQbeT ty <+> "%" <> fromText name

toQbeParam (e, ty) = toQbeT ty <+> toQbeE e

-- TODO add size for arrays
-- in C-style the size is after the name
-- int a[5] instead of int[5] a
toQbeStructParam (Name _ ty) = toQbeT ty

--toQbeE (Variable name _) = "%" <> fromText name
--toQbeE (Variable name _) = "$" <> fromText name
toQbeE (Variable name _) = fromName name
toQbeE (Literal l) = toQbeL l
toQbeE other = error ("QbeE " ++ show other)

toQbeL (Int32 l) = fromText (pack (show l))
toQbeL (Int64 l) = fromText (pack (show l))
toQbeL (Float32 l) = fromText (pack (show l))
toQbeL (Float64 l) = fromText (pack (show l))
toQbeL (Bool True) = "true"
toQbeL (Bool False) = "false"
toQbeL (StringLiteral s) = "\"" <> fromText (escape s) <> "\""

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
toQbeT (Concrete "char" []) = "b"
toQbeT (Concrete "void" []) = " "
toQbeT (Concrete "int" []) = "w"
toQbeT (Concrete "long" []) = "l"
toQbeT (Concrete "ptr" []) = "l"
toQbeT (Concrete "auto" []) = error "Error: auto appeared in printing stage"
toQbeT (Concrete s []) = ":" <> fromText s
toQbeT (Concrete s typeParameters) =
    error ("Error: generic type "
        ++ unpack s ++ show typeParameters ++ " appeared in printing stage")



toCs (Module _ s) = intercalate "\n\n" (fmap toCsS s)


toCsS (Definition (Name name ty) e) =
    toCsT ty <+> fromText name <+> "=" <+> toCsE e <> ";"
toCsS (Call e) = toCsE e <> ";"
toCsS (Assignment e1 e2) = toCsE e1 <+> "=" <+> toCsE e2 <> ";"
toCsS (FunctionDefintion name typeParameters ty parameters statements) =
    "public static" <+> toCsT ty <+> fromText name <> toCsTypDefParams typeParameters
        <> parens (intercalate ", " (fmap toCsFunParam parameters))
        <//> "{"
        <//> indent (intercalate "\n" (fmap toCsS statements))
        <//> "}"
toCsS (Return (Just e)) = "return" <+> toCsE e <> ";"
toCsS (Return Nothing) = "return;"
toCsS (Import _ _) = ""
toCsS (ExternDefintion name returnType parameters) =
    "extern" <+> toCsT returnType <+> fromText name <> parens (intercalate ", " (fmap toCsFunParam parameters)) <>";"
toCsS (StructDefinition name typeParameters parameters) =
    "public struct" <+> fromText name <> toCsTypDefParams typeParameters
        <//> "{"
        <//> indent (intercalate "\n" (fmap toCsStructParam parameters))
        <//> "}"
toCsS (If (cond:conds) Nothing) =
    printIf cond conds
toCsS (If (cond:conds) (Just th)) =
    printIf cond conds <//> printElsePart th
toCsS (While cond sts) = "while" <+> parens (toCsE cond)
    <//> printBlock sts
toCsS other = error ("Error: CsS Following statement appearedd in printing stage " ++ show other)

printIf cond conds = intercalate "\n" (printIfPart cond : fmap printElseIfPart conds)

printIfPart (cond, sts) = "if" <+> parens (toCsE cond)
    <//> printBlock sts

printElseIfPart cond = "else" <+> printIfPart cond

printElsePart sts = "else" <//> printBlock sts

printBlock sts = "{"
    <//> indent (intercalate "\n" (fmap toCsS sts))
    <//> "}"

toCsFunParam (Name name ty) = toCsT ty <+> fromText name

toCsTypDefParams [] = mempty
toCsTypDefParams tyNames = "<" <> intercalate ", " (fmap fromText tyNames) <> ">"

toCsTypParams [] = mempty
toCsTypParams typeParameters = "<" <> intercalate ", " (fmap toCsT typeParameters) <> ">"

toCsParam (e, ty) = toCsT ty <+> toCsE e

toCsStructParam (Name name ty) = "public" <+> toCsT ty <+> fromText name <> ";"

toCsE (Variable name typeParameters) = fromName name <> toCsTypParams typeParameters
toCsE (Literal l) = toCsL l
-- Operators
toCsE (Apply (Variable (Name v _) _) [e1, e2]) | isOperator v =
    parensToCsE e1 <+> fromText v <+> parensToCsE e2
-- Non Operator Apply
toCsE (Apply e es) = toCsE e <> parens (intercalate ", " (fmap toCsE es))
toCsE (DotAccess e name typeParameters) =
    toCsE e <> "." <> fromName name <> toCsTypParams typeParameters
toCsE (SquareAccess e1 e2) =
    toCsE e1 <> "[" <> toCsE e2 <> "]"
toCsE (ArrayExpression es) =
    "[" <> intercalate ", " (fmap toCsE es) <> "]"

fromName (Name n _) = fromText n

toCsL (Int32 l) = fromText (pack (show l))
toCsL (Int64 l) = fromText (pack (show l)) <> "L"
toCsL (Float32 l) = fromText (pack (show l)) <> "f"
toCsL (Float64 l) = fromText (pack (show l))
toCsL (Bool True) = "true"
toCsL (Bool False) = "false"
toCsL (StringLiteral s) = "\"" <> fromText (escape s) <> "\""

parensToCsE e =
    case e of
        (Apply (Variable (Name i _) _) _) | isOperator i -> parens (toCsE e)
        _ -> toCsE e

toCsT (ArrayType t _) = toCsT t <> "[]"
toCsT t@(Concrete s typeParameters) =
    if t == auto
        then "var"
        else fromText s <> toCsTypParams typeParameters

parens x = "(" <> x <> ")"
