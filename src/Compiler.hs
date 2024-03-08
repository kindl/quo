{-# LANGUAGE OverloadedStrings #-}
module Compiler where

import Types
import Control.Monad((>=>))
import Control.Monad.Trans.Reader(runReaderT, asks)
import Control.Monad.Trans.Class(lift)
import Data.IORef
import Platte
import Data.Text(Text, pack, intercalate, isSuffixOf, replace)


data Env = Env (IORef Int) (IORef [Statement]) (IORef [(Text, Type)])

insert key value env = (key, value) : env

newUnique = do
    ref <- asks (\(Env supply _ _) -> supply)
    lift (modifyIORef' ref succ >> readIORef ref)

-- Transformations
runTransformations m = do
    i <- newIORef 0
    env <- newIORef []
    tyEnv <- newIORef []

    result <- runReaderT (transformations m) (Env i env tyEnv)
    statements <- readIORef env

    return (statements ++ result)

transformations = toM normalizeTypeNames
    >=> makeStringsGlobal
    >=> transformIfIntoJumps
    >=> transformWhileIntoJumps
    >=> toM makeFunctionsGlobal
    >=> transformCallIntoDef
    >=> ssa
    >=> autoIntoType

toM f x = return (f x)


normalizeTypeNames m =
    let
        f (TypeVariable "int" []) = TypeVariable "i32" []
        f (TypeVariable "long" []) = TypeVariable "i64" []
        f (TypeVariable "uint" []) = TypeVariable "u32" []
        f (TypeVariable "ulong" []) = TypeVariable "u64" []
        f x = x
    in transformBi f m

-- Create a new variable, put the string into global definitions and swap
makeStringsGlobal m =
    let
        f s@(String _) = do
            n <- fmap (\i -> pack ("s" ++ show i)) newUnique
            ref <- asks (\(Env _ env _) -> env)
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

            return ([Definition condVar (TypeVariable "bool" []) cond,
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
                Definition condVar (TypeVariable "bool" []) cond,
                JumpNonZero condVar loopVar breakVar,
                Label loopVar] ++ loopStats ++ [Jump continueVar, Label breakVar] ++ statements)
        f x = return x
    in transformBiM f m

-- IDEA for generic
-- * grab all generic function and struct definitions
-- * go into all function definitions without type parameters (leafs)
-- * look for calls of generic functions and uses of genric structs
-- * instantiate their specialization
-- * exchange generic calls with calls to the specialization
-- * for example alloc<int>() is transformed into alloc__int() and specialization alloc__int is created
-- * exchange use of generic structs with use of the specialization
-- * repeat for the specializations, because they also contain generic calls
-- * some generic calls have to be built-in. For example sizeof<int>() or cast<int>(v)

-- Turns auto into concrete types
autoIntoType m =
    let
        f (Definition name returnType (Apply _ e@(Variable _ v []) es)) = do
            ref <- asks (\(Env _ _ tyEnv) -> tyEnv)
            env <- lift (readIORef ref)
            if v == "!=" || v == "=="
                -- TODO operators: get type from paramters and pick correct call ceqw, ceql etc.
                then return (Definition name (TypeVariable "bool" []) (Apply auto e es))
                else (case lookup v env of
                    -- TODO match returnType with (last ts)
                    Just t@(TypeVariable "Func" ts) -> return (Definition name (last ts) (Apply t e es))
                    Nothing -> fail ("Unknown " ++ show v ++ " in " ++ show (fmap fst env))
                    Just t -> fail ("Non function type " ++ show t ++ " for " ++ show v ++ " in " ++ show (fmap fst env)))
        f (FunctionDefintion name returnType [] parameters statements) = do
            addToEnv name (TypeVariable "Func" (fmap snd parameters ++ [returnType]))
            statements' <- traverse f statements
            return (FunctionDefintion name returnType [] parameters statements')
        f s@(ExternDefintion name returnType parameters) = do
            addToEnv name (TypeVariable "Func" (fmap snd parameters ++ [returnType]))
            return s
        f s = return s
    in traverse f m

addToEnv name ty = do
    ref <- asks (\(Env _ _ tyEnv) -> tyEnv)
    lift (modifyIORef' ref (insert name ty))
    return ()


-- Pretty Printing to Qbe format
toQbe s = intercalate "\n\n" (fmap toQbeP s)

-- Top level
toQbeP (Definition name ty e) =
    "data" <+> "$" <> name <+> "=" <+> "{" <+> toQbeT ty <+> toQbeE e <+> "}"
toQbeP s = toQbeS s

-- TODO use type in comparisons for float and similar
toQbeS (Definition name ty (Apply t (Variable _ "==" _) [e1, e2])) =
    indent <> "%" <> name <+> "=" <> toQbeT ty <+> "ceqw" <+> toQbeE e1 <> "," <+> toQbeE e2
toQbeS (Definition name ty (Apply t (Variable _ "!=" _) [e1, e2])) =
    indent <> "%" <> name <+> "=" <> toQbeT ty <+> "cnew" <+> toQbeE e1 <> "," <+> toQbeE e2
-- No variable necessary for void
toQbeS (Definition _ (TypeVariable "void" []) (Apply ty v parameters)) =
    indent <> makeCall ty v parameters
toQbeS (Definition name returnType (Apply ty v parameters)) =
    indent <> "%" <> name <+> "=" <> toQbeT returnType <+> makeCall ty v parameters
toQbeS (Definition name ty e) =
    indent <> "%" <> name <+> "=" <> toQbeT ty <+> toQbeE e
toQbeS (FunctionDefintion name ty _ parameters statements) =
    "export function" <+> toQbeT ty <+> "$" <> name <> "(" <> intercalate ", " (fmap toQbeFunParam parameters) <> ")" <+> "{\n"
    <> "@start\n"
    <> intercalate "\n" (fmap toQbeS statements)
    <> "\n}"
toQbeS (Return (Just e)) = indent <> "ret" <+> toQbeE e
toQbeS (Return Nothing) = indent <> "ret"
toQbeS (JumpNonZero c t e) = indent <> "jnz" <+> "%" <> c <> "," <+> "@" <> t <> "," <+> "@" <> e
toQbeS (Jump l) = indent <> "jmp" <+> "@" <> l
toQbeS (Label e) = "@" <> e
-- TODO filter these out, because they create a lot of whitespace
toQbeS (Import _ _) = ""
toQbeS (ExternDefintion _ _ _) = ""
toQbeS (StructDefinition name _ parameters) = "type" <+> ":" <> name <+> "=" <+> "{" <+> intercalate ", " (fmap toQbeStructParam parameters) <+> "}"
toQbeS other = error ("QbeS Following statement should no longer exist at this stage " ++ show other)

makeCall (TypeVariable "Func" tys) v parameters =
    let
        parametersWithType = zip parameters tys
    in "call" <+> toQbeE v <> "(" <> intercalate ", " (fmap toQbeParam parametersWithType) <> ")"
makeCall t v _ = error ("Non-function type in makeCall " ++ show t ++ " calling " ++ show v)

toQbeFunParam (name, ty) = toQbeT ty <+> "%" <> name

toQbeParam (e, ty) = toQbeT ty <+> toQbeE e

-- TODO add size for arrays
toQbeStructParam (name, ty) = toQbeT ty

toQbeE (Variable Local name _) = "%" <> name
toQbeE (Variable Global name _) = "$" <> name
toQbeE (Int64 l) = pack (show l)
toQbeE (Float64 l) = pack (show l)
toQbeE (Boolean True) = "true"
toQbeE (Boolean False) = "false"
toQbeE (String s) = "\"" <> escape s <> "\""
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
-- TODO Is bool b or w?
toQbeT (TypeVariable "bool" []) = "w"
toQbeT (TypeVariable "i32" []) = "w"
toQbeT (TypeVariable "i64" []) = "l"
toQbeT (TypeVariable "auto" []) = error "auto in late stage"
-- TODO the following match should look more like this:
-- toQbeT (TypeVariable s []) =
-- because type parameters should not exist at this point.
-- Currently this is necessary for constructs like
-- `UnsafePointer<char>`
toQbeT (TypeVariable s _) =
    if isSuffixOf "Pointer" s
        then pointerLength
        -- Add : sigil for structs
        else ":" <> s

pointerLength = "l"

indent = "    "

(<+>) a b = a <> " " <> b
