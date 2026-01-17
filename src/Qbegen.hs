{-# LANGUAGE OverloadedStrings #-}
module Qbegen(moduleToQbe, prettyMod) where

import Data.Text(Text, pack, isPrefixOf)
import Types
import Resolver(StructLookup, TypeLookup, readType, gatherStructs)
import Data.IORef(IORef, modifyIORef', newIORef, readIORef)
import Control.Monad(when, zipWithM_)
import Control.Monad.Trans.Reader(ReaderT, runReaderT, asks, local)
import Control.Monad.Trans.Class(lift)
import Data.Foldable(traverse_)
import Data.Int(Int32)
import Prettyprinter((<+>), Doc, parens)
import Helpers((<//>), indent, fromText, intercalate, escape, isConstructor)


-- Ident does not contain a sigil, while Val does
type Ident = Text

type Label = Ident

type Val = Text

type Ty = Text

data Mod = Mod Ident [Def]

data Block =
    Instruction Ident Ty Ident [Val]
    | CallInstruction Ident Ty Val [(Ty, Val)]
    | Store Ty Val Val
    | Label Ident
    | Jump Label
    | JumpNonZero Val Label Label
    | Ret (Maybe Val)

data Def =
    DataDef Ident [(Ty, [Val])]
    | TypeDef Ident [Ty]
    | FuncDef Ty Ident [(Ty, Ident)] [Block]

data EmitEnv = EmitEnv {
    -- List of global names and parameter names for determining sigil
    getGlobals :: [Text],
    getParameters :: [Text],
    getNameSupply :: IORef Int,
    getStringLiterals :: IORef [Def],
    getStructs :: StructLookup,
    -- Per function
    getAllocs :: IORef [Block],
    getInstructions :: IORef [Block],
    -- Per while block
    getLoopLabels :: Maybe (Label, Label)
}

type Emit a = ReaderT EmitEnv IO a

numberToVal :: Int32 -> Val
numberToVal = pack . show

newUnique :: Emit Int
newUnique = do
    ref <- asks getNameSupply
    lift (modifyIORef' ref succ >> readIORef ref)

newIdent :: Text -> Emit Text
newIdent prefix = fmap (\i -> prefix <> pack (show i)) newUnique

emit :: Block -> Emit ()
emit instruction = do
    ref <- asks getInstructions
    lift (modifyIORef' ref (\instructions -> instructions ++ [instruction]))

emitAlloc :: Text -> Type -> Emit ()
emitAlloc name ty = do
    ref <- asks getAllocs
    size <- getSizeAsVal ty
    alignment <- getAlignmentAsText ty
    let instruction = Instruction name pointerTy ("alloc" <> alignment) [size]
    lift (modifyIORef' ref (\instructions -> instructions ++ [instruction]))

withParameters :: [Text] -> Emit a -> Emit a
withParameters params action =
    local (\emitEnv -> emitEnv { getParameters = params }) action

withLoopLabels :: (Label, Label) -> Emit a -> Emit a
withLoopLabels loopLabels =
    local (\emitEnv -> emitEnv { getLoopLabels = Just loopLabels })

withFreshEmitEnv :: Emit () -> Emit [Block]
withFreshEmitEnv action = do
    allocs <- lift (newIORef [])
    instructions <- lift (newIORef [])
    local (\emitEnv -> emitEnv { getAllocs = allocs, getInstructions = instructions}) action
    a <- lift (readIORef allocs)
    i <- lift (readIORef instructions)
    return (a ++ i)

isGlobal :: Text -> Emit Bool
isGlobal name = do
    globalNames <- asks getGlobals
    return (elem name globalNames)

isParam :: Text -> Emit Bool
isParam name = do
    params <- asks getParameters
    return (elem name params)

toBlock :: [Statement] -> Emit ()
toBlock = traverse_ statementToBlock

bodyToBlock :: [Text] -> [Statement] -> Emit ()
bodyToBlock parameters statements =
    withParameters parameters (toBlock statements)

statementToBlock :: Statement -> Emit ()
statementToBlock (Return Nothing) =
    emit (Ret Nothing)
statementToBlock (Return (Just val)) = do
    returnVal <- expressionToVal val
    emit (Ret (Just returnVal))
statementToBlock (Call expression) =
    expressionToVal expression >> return ()
statementToBlock (Definition name expression) =
    definitionToBlocks name expression
statementToBlock (Assignment (Variable var _) rightHand) = do
    -- TODO handling complex leftHand expressions
    let name = getInnerText var
    let ty = getType var
    wasParam <- isParam name
    when wasParam (fail ("Parameter " ++ show name ++ " is constant and cannot be reassigned"))
    wasGlobal <- isGlobal name
    when wasGlobal (fail ("Global " ++ show name ++ " is constant and cannot be reassigned"))
    val <- expressionToVal rightHand
    emitStore ty val ("%" <> name)
statementToBlock (If branches maybeElseBranch) = do
    endLabel <- newLabel "end"
    emitBranches endLabel branches maybeElseBranch
{-
@continue
    %cond = cond
    jnz %cond, @loop, @break
@loop
    sts1
    jmp @continue
@break
    ...
-}
statementToBlock (While condition statements) = do
    continueLabel <- newLabel "continue"
    loopLabel <- newLabel "loop"
    breakLabel <- newLabel "break"

    emit (Label continueLabel)
    val <- expressionToVal condition
    emit (JumpNonZero val loopLabel breakLabel)
    emit (Label loopLabel)
    withLoopLabels (breakLabel, continueLabel) (toBlock statements)
    emit (Jump continueLabel)
    emit (Label breakLabel)
statementToBlock BreakStatement = do
    maybeLabels <- asks getLoopLabels
    case maybeLabels of
        Nothing -> fail "Cannot use break outside loops"
        Just (breakLabel, _) -> emit (Jump breakLabel)
statementToBlock ContinueStatement = do
    maybeLabels <- asks getLoopLabels
    case maybeLabels of
        Nothing -> fail "Cannot use continue outside loops"
        Just (_, continueLabel) -> emit (Jump continueLabel)
statementToBlock s = fail ("Cannot compile statement " ++ show s)

definitionToBlocks :: Name -> Expression -> Emit ()
definitionToBlocks nameWithType (Apply (Variable constructor []) expressions) | isConstructor constructor = do
    -- This special case exists so that assigning new structs does not allocate twice,
    -- one alloc for the definition and one for the expression
    let name = getInnerText nameWithType
    let structType = getType nameWithType
    emitAlloc name structType
    emitAssignFields structType ("%" <> name) expressions
definitionToBlocks nameWithType (Apply (Variable name []) [cond, e1, e2]) | getInnerText name == "?" = do
    let name = getInnerText nameWithType
    let resultType = getType nameWithType
    emitAlloc name resultType
    emitIfExpression' resultType ("%" <> name) cond e1 e2
definitionToBlocks nameWithType (ArrayExpression expressions) =
    case getType nameWithType of
        ArrayType elementType _ -> do
            let name = getInnerText nameWithType
            emitAlloc name (ArrayType elementType (Just (fromIntegral (length expressions))))
            emitAssignElements elementType ("%" <> name) expressions
        _ -> fail ("Unexpected non-array type " <> show nameWithType)
definitionToBlocks nameWithType expression = do
    -- The allocation for locals are not emitted here, but per function.
    -- This is necessary so that we do not keep reallocating,
    -- for example in a while-loop
    let name = getInnerText nameWithType
    let ty = getType nameWithType
    emitAlloc name ty
    val <- expressionToVal expression
    emitStore ty val ("%" <> name)

emitBranches :: Ident -> [(Expression, [Statement])] -> Maybe [Statement] -> Emit ()
emitBranches endLabel [] maybeElseBranch = do
    toBlock (concat maybeElseBranch)
    emit (Label endLabel)
emitBranches endLabel ((condition, statements):rest) maybeElseBranch = do
    thenLabel <- newLabel "then"
    elseLabel <- newLabel "else"

    val <- expressionToVal condition
    emit (JumpNonZero val thenLabel elseLabel)
    emit (Label thenLabel)
    toBlock statements
    -- Only emit a jump if there is no ret/jmp
    -- It is a syntax error to have a jmp after a ret.
    if not (null statements) && isJumpStatement (last statements)
        then return ()
        else emit (Jump endLabel)

    emit (Label elseLabel)
    emitBranches endLabel rest maybeElseBranch

getSizeAsVal :: Type -> Emit Val
getSizeAsVal ty = do
    structEnv <- asks getStructs
    let size = getSize structEnv ty
    return (numberToVal size)

-- This does not produce a val,
-- it is necessary for instruction names alloc4 alloc8 etc.
getAlignmentAsText :: Type -> Emit Text
getAlignmentAsText ty = do
    structEnv <- asks getStructs
    let alignment = getAlignment structEnv ty
    return (pack (show alignment))

findFields :: StructLookup -> Type -> TypeLookup
findFields structEnv (Concrete structName []) =
    case lookup (getText structName) structEnv of
        Nothing -> error ("Qbegen: Cannot get fields of struct " ++ show structName)
        Just fields -> fields
findFields structEnv (PointerType innerTy) =
    findFields structEnv innerTy
findFields _ ty =
    error ("Qbegen: Cannot get field of non-struct type " ++ show ty)

getOffset :: Type -> Text -> Emit Int32
getOffset structType name = do
    structEnv <- asks getStructs
    let fields = findFields structEnv structType
    return (getOffset' structEnv name fields)

getOffset' :: [(Text, TypeLookup)] -> Text -> [(Text, Type)] -> Int32
getOffset' _ name [] = error ("Did not encounter field " ++ show name)
getOffset' structEnv name ((fieldName, ty):fields) =
    if name == fieldName
        then 0
        else
            let
                s = getSize structEnv ty
                o = getOffset' structEnv name fields
            in (s + o)

annotateOffsets :: Type -> Emit [(Text, (Type, Int32))]
annotateOffsets ty = do
    structEnv <- asks getStructs
    let fields = findFields structEnv ty
    return (annotateOffsets' structEnv 0 fields)

annotateOffsets' :: [(Text, TypeLookup)] -> Int32 -> [(a, Type)] -> [(a, (Type, Int32))]
annotateOffsets' _ _ [] = []
annotateOffsets' structEnv current ((fieldName, ty):fields) =
    let
        s = getSize structEnv ty
        rest = annotateOffsets' structEnv (current + s) fields
    in ((fieldName, (ty, current)):rest)

pointerSize :: Int32
pointerSize = 8

-- TODO do we need to return Word64?
-- When are structs that big?
getSize :: StructLookup -> Type -> Int32
getSize structEnv (Concrete name []) =
    case getText name of
        "bool" -> 1
        "char" -> 1
        "short" -> 2
        "ushort" -> 2
        "int" -> 4
        "uint" -> 4
        "long" -> 8
        "ulong" -> 8
        "float" -> 4
        "double" -> 8
        "usize" -> 8
        "string" -> pointerSize
        structName ->
            case lookup structName structEnv of
                Nothing ->
                    error ("Cannot determine size of type " ++ show structName
                        ++ " because it was not in the environment" ++ show structEnv)
                Just fields ->
                    let sizes = fmap (getSize structEnv . snd) fields
                    in sum sizes
getSize structEnv (ArrayType ty (Just arraySize)) =
    let elementSize = getSize structEnv ty
    in elementSize * arraySize
getSize _ (PointerType _) = pointerSize
getSize _ other = error ("Cannot determine size of type " ++ show other)

-- This function is very similar to getSize, can the two be merged?
getAlignment :: StructLookup -> Type -> Int32
getAlignment structEnv (Concrete i []) =
    case getText i of
        "bool" -> 1
        "char" -> 1
        "short" -> 2
        "ushort" -> 2
        "int" -> 4
        "uint" -> 4
        "long" -> 8
        "ulong" -> 8
        "float" -> 4
        "double" -> 8
        "usize" -> 8
        "string" -> 8
        structName ->
            case lookup structName structEnv of
                Nothing ->
                    error ("Cannot determine alignment of type " ++ show structName
                        ++ " because it was not in the environment" ++ show structEnv)
                Just fields ->
                    let alignments = fmap (getAlignment structEnv . snd) fields
                    in maximum alignments
getAlignment _ (PointerType _) = 8

expressionToVal :: Expression -> Emit Val
expressionToVal (Variable var []) | getInnerText var == "nullptr" =
    return "0"
expressionToVal (Variable var []) = do
    let name = getInnerText var
    let ty = getType var
    wasParam <- isParam name
    wasGlobal <- isGlobal name
    let sigil = if wasGlobal then "$" else "%"
    if wasParam
        then return (sigil <> name)
        else emitLoadOrVal ty (sigil <> name)
expressionToVal (Literal l) =
    literalToVal l
expressionToVal (Apply expression expressions) =
    emitApply expression expressions
expressionToVal (SquareAccess expression accessor) = do
    -- This will hold the accessor converted to size
    extVal <- newIdent ".local"
    -- This will hold the accessor times element size
    mulVal <- newIdent ".local"
    -- This will hold the offsetted pointer
    offsetVal <- newIdent ".local"
    val <- expressionToVal expression
    accessorVal <- expressionToVal accessor
    let (ArrayType elementType _) = readType expression
    elementSize <- getSizeAsVal elementType

    emit (Instruction extVal pointerTy "extsw" [accessorVal])
    emit (Instruction mulVal pointerTy "mul" ["%" <> extVal, elementSize])
    emit (Instruction offsetVal pointerTy "add" [val, "%" <> mulVal])

    emitLoadOrVal elementType ("%" <> offsetVal)
expressionToVal (DotAccess expression (Name fieldName fieldType) []) = do
    val <- expressionToVal expression
    let structType = readType expression
    offset <- getOffset structType (getText fieldName)
    offsetVal <- createOffsetVal offset val
    emitLoadOrVal fieldType offsetVal

-- TODO Make the evaluation of parameters more visible
emitApply :: Expression -> [Expression] -> Emit Text
emitApply (Variable name _) [cond, thenExpression, elseExpression] | getInnerText name == "?" =
    case getType name of
        FunctionType returnType _ ->
            emitIfExpression returnType cond thenExpression elseExpression
        _ -> fail ("Unexpected non-function type " <> show name)
emitApply (Variable name _) [parameter1, parameter2] | getInnerText name == "&&" =
    emitIfExpression boolType parameter1 parameter2 (Literal (Bool False))
emitApply (Variable name _) [parameter1, parameter2] | getInnerText name == "||" =
    emitIfExpression boolType parameter1 (Literal (Bool True)) parameter2
emitApply (Variable name [targetType]) [parameter] | getInnerText name == "cast" = do
    let parameterType = readType parameter
    val <- expressionToVal parameter
    emitCast parameterType targetType val
emitApply (Variable name [typeParameter]) _ | getInnerText name == "sizeof" =
    getSizeAsVal typeParameter
emitApply (Variable n@(Name _ (FunctionType structType _)) []) expressions | isConstructor n = do
    ident <- newIdent ".local"
    emitAlloc ident structType
    emitAssignFields structType ("%" <> ident) expressions
    return ("%" <> ident)
-- TODO try to combine the following cases if possible
emitApply (Variable var []) expressions =
    case getType var of
        FunctionType returnType parameterTypes -> do
            freshIdent <- newIdent ".local"
            vals <- traverse expressionToVal expressions
            let retTy = toQbeWordTy returnType
            let parameterTys = fmap toQbeTy parameterTypes
            let zipped = zip parameterTys vals
            let name = getInnerText var
            if isOperator name
                then emitOperator freshIdent retTy name zipped
                else do
                    wasGlobal <- isGlobal name
                    let sigil = if wasGlobal then "$" else "%"
                    emit (CallInstruction freshIdent retTy (sigil <> name) zipped)
            return ("%" <> freshIdent)
        _ -> fail ("Unexpected non-function type " <> show var)
-- TODO Should the following case be allowed? For example for currying?
-- Probably not very interesting without closures, but possible
-- getCompare(compareOption)(a, b)
emitApply expression expressions = do
    freshIdent <- newIdent ".local"
    val <- expressionToVal expression
    vals <- traverse expressionToVal expressions
    when (not (isPrefixOf "$" val)) (fail ("Is not a global function " ++ show val))
    let (FunctionType returnType parameterTypes) = readType expression
    let retTy = toQbeWordTy returnType
    let parameterTys = fmap toQbeTy parameterTypes
    let zipped = zip parameterTys vals
    emit (CallInstruction freshIdent retTy val zipped)
    return ("%" <> freshIdent)

emitOperator :: Ident -> Ty -> Text -> [(Text, Val)] -> Emit ()
emitOperator ident qbeTy "-_" [(_, val)] =
    emit (Instruction ident qbeTy "neg" [val])
-- TODO How is bitwise not usually expressed?
emitOperator ident qbeTy "~" [(_, val)] =
    emit (Instruction ident qbeTy "xor" [val, "-1"])
emitOperator ident qbeTy "!" [(_, val)] =
    emit (Instruction ident qbeTy "ceqw" [val, "0"])
emitOperator ident qbeTy op [(qbeTy1, val1), (qbeTy2, val2)] =
    if qbeTy1 == qbeTy2
        then emitOperator' ident qbeTy op qbeTy1 val1 val2
        else fail ("Emit operator: Wrong type for operator " ++ show op ++ " " ++ show qbeTy1 ++ " " ++ show qbeTy2)
emitOperator _ _ _ _ =
    fail "Emit operator: Wrong args"

emitOperator' :: Ident -> Ty -> Text -> Ident -> Val -> Val -> Emit ()
emitOperator' ident qbeTy "+" _ val1 val2 =
    emit (Instruction ident qbeTy "add" [val1, val2])
emitOperator' ident qbeTy "-" _ val1 val2 =
    emit (Instruction ident qbeTy "sub" [val1, val2])
emitOperator' ident qbeTy "*" _ val1 val2 =
    emit (Instruction ident qbeTy "mul" [val1, val2])
emitOperator' ident qbeTy "/" _ val1 val2 =
    emit (Instruction ident qbeTy "div" [val1, val2])
emitOperator' ident qbeTy "&" argQbeTy val1 val2 =
    emit (Instruction ident qbeTy "and" [val1, val2])
emitOperator' ident qbeTy "|" argQbeTy val1 val2 =
    emit (Instruction ident qbeTy "or" [val1, val2])
emitOperator' ident qbeTy "^" argQbeTy val1 val2 =
    emit (Instruction ident qbeTy ("xor" <> argQbeTy) [val1, val2])
emitOperator' ident qbeTy "==" argQbeTy val1 val2 =
    emit (Instruction ident qbeTy ("ceq" <> argQbeTy) [val1, val2])
emitOperator' ident qbeTy ">" argQbeTy val1 val2 =
    emit (Instruction ident qbeTy ("csgt" <> argQbeTy) [val1, val2])
emitOperator' ident qbeTy "<" argQbeTy val1 val2 =
    emit (Instruction ident qbeTy ("cslt" <> argQbeTy) [val1, val2])
emitOperator' ident qbeTy "<=" argQbeTy val1 val2 =
    emit (Instruction ident qbeTy ("csle" <> argQbeTy) [val1, val2])
emitOperator' ident qbeTy ">=" argQbeTy val1 val2 =
    emit (Instruction ident qbeTy ("csge" <> argQbeTy) [val1, val2])
emitOperator' ident qbeTy "!=" argQbeTy val1 val2 =
    emit (Instruction ident qbeTy ("cne" <> argQbeTy) [val1, val2])
emitOperator' _ _ op _ _ _ = fail ("Unknown operator " <> show op)

emitAssignExpression :: Val -> Type -> Expression -> Emit ()
-- Nested struct creation:
-- When we have a struct Vector { int x, int y }
-- and struct Matrix { Vector v, Vector w }
-- and build it with Matrix(Vector(1, 2), Vector(3, 4))
-- then we don't want to allocate the Vectors, we just want to allocate Matrix
-- and assign the individual ints.
emitAssignExpression targetVal fieldType (Apply (Variable name []) expressions) | isConstructor name =
    emitAssignFields fieldType targetVal expressions
emitAssignExpression targetVal (ArrayType elementType _) (ArrayExpression expressions) =
    emitAssignElements elementType targetVal expressions
-- Nested struct copying:
-- Matrix(Vector2Int(x, y), makeVector2Int(z))
-- After creating a value, fields are copied one by one
-- this probably could be reused for assigning structs
-- TODO there are many special cases for expression to val,
-- which does not alloc for structs. Maybe these special cases can be collapsed
emitAssignExpression targetVal fieldType expression = do
    val <- expressionToVal expression
    -- TODO handle strings
    emitStore fieldType val targetVal

emitAssignElements :: Type -> Val -> [Expression] -> Emit ()
emitAssignElements elementType target expressions = do
    let numbers = [0 .. fromIntegral (length expressions)]
    structEnv <- asks getStructs
    let elementSize = getSize structEnv elementType
    let pairs = fmap (\index -> (elementType, index * elementSize)) numbers
    zipWithM_ (emitAssignAux target) pairs expressions

emitIfExpression' :: Type -> Val -> Expression -> Expression -> Expression -> Emit ()
emitIfExpression' resultType target cond thenExpression elseExpression = do
    thenLabel <- newLabel "then"
    elseLabel <- newLabel "else"
    endLabel <- newLabel "end"

    val <- expressionToVal cond
    emit (JumpNonZero val thenLabel elseLabel)
    emit (Label thenLabel)
    thenVal <- expressionToVal thenExpression
    emitStore resultType thenVal target
    emit (Jump endLabel)

    emit (Label elseLabel)
    elseVal <- expressionToVal elseExpression
    emitStore resultType elseVal target
    emit (Label endLabel)

emitIfExpression :: Type -> Expression -> Expression -> Expression -> Emit Val
emitIfExpression resultType cond thenExpression elseExpression = do
    targetVal <- newIdent ".local"
    emitAlloc targetVal resultType
    emitIfExpression' resultType ("%" <> targetVal) cond thenExpression elseExpression
    emitLoadOrVal resultType ("%" <> targetVal)

emitAssignFields :: Type -> Val -> [Expression] -> Emit ()
emitAssignFields structType target expressions = do
    annotated <- annotateOffsets structType
    let pairs = fmap snd annotated
    zipWithM_ (emitAssignAux target) pairs expressions

emitAssignAux :: Text -> (Type, Int32) -> Expression -> Emit ()
emitAssignAux target (ty, off) expression = do
    offsetVal <- createOffsetVal off target
    emitAssignExpression offsetVal ty expression

createOffsetVal :: Int32 -> Text -> Emit Text
createOffsetVal offset val = do
    if offset == 0
        then return val
        else do
            offsetted <- newIdent ".local"
            let offsetVal = pack (show offset)
            emit (Instruction offsetted pointerTy "add" [val, offsetVal])
            return ("%" <> offsetted)

emitStore :: Type -> Val -> Val -> Emit ()
emitStore ty val mem =
    if isPrimitive ty || isPointerType ty
        then emit (Store (toQbeStoreTy ty) val mem)
        else emitStoreFields ty val mem

emitStoreFields :: Type -> Val -> Val -> Emit ()
emitStoreFields structType val mem = do
    annotated <- annotateOffsets structType
    let pairs = fmap snd annotated
    traverse_ (emitStoreFieldsAux val mem) pairs

emitStoreFieldsAux :: Val -> Val -> (Type, Int32) -> Emit ()
emitStoreFieldsAux val mem (fieldType, offset) = do
    valOffsetVal <- createOffsetVal offset val
    memOffsetVal <- createOffsetVal offset mem
    loadedVal <- emitLoadOrVal fieldType valOffsetVal
    emitStore fieldType loadedVal memOffsetVal

emitLoadOrVal :: Type -> Text -> Emit Text
emitLoadOrVal ty val =
    if isPrimitive ty || isPointerType ty
        -- when accessing an int[] a with a[1] the result will be an int and loaded into a temporary
        then emitLoad ty val
        -- when accessing some someStruct[] a with a[1] the result will be a pointer to the struct, no load neccessary
        else return val

emitLoad :: Type -> Val -> Emit Text
emitLoad ty val = do
    let broadQbeTy = toQbeWordTy ty
    let qbeTy = toQbeTy ty
    loaded <- newIdent ".local"
    emit (Instruction loaded broadQbeTy ("load" <> qbeTy) [val])
    if broadQbeTy == qbeTy
        then return ("%" <> loaded)
        else do
            extended <- newIdent ".local"
            emit (Instruction extended broadQbeTy ("ext" <> qbeTy) ["%" <> loaded])
            return ("%" <> extended)

-- TODO remove overlap by finding general rules
emitCast :: Type -> Type -> Text -> Emit Text
emitCast (PointerType _) (PointerType _) val = return val
emitCast (Concrete fromName []) targetType@(Concrete targetName []) val =
    case (getText fromName, getText targetName) of
        (a, b) | a == b -> return val
        ("int", "uint") -> return val
        ("uint", "int") -> return val
        ("int", "ushort") -> return val
        ("int", "short") -> return val
        ("int", "char") -> return val
        ("ulong", "int") -> return val
        ("int", "ulong") -> return val
        ("long", "int") -> return val
        ("ulong", "long") -> return val
        ("long", "ulong") -> return val
        -- TODO these probably work on smaller types, e.g. char to long
        ("uint", "long") -> do
            freshIdent <- newIdent ".local"
            emit (Instruction freshIdent "l" "extuw" [val])
            return ("%" <> freshIdent)
        ("int", "long") -> do
            freshIdent <- newIdent ".local"
            emit (Instruction freshIdent "l" "extsw" [val])
            return ("%" <> freshIdent)
        ("char", _) | isIntegerType targetType -> do
            freshIdent <- newIdent ".local"
            emit (Instruction freshIdent (toQbeTy targetType) "extub" [val])
            return ("%" <> freshIdent)
        ("ushort", _) | isIntegerType targetType -> do
            freshIdent <- newIdent ".local"
            emit (Instruction freshIdent (toQbeTy targetType) "extuh" [val])
            return ("%" <> freshIdent)
        ("short", _) | isIntegerType targetType -> do
            freshIdent <- newIdent ".local"
            emit (Instruction freshIdent (toQbeTy targetType) "extsh" [val])
            return ("%" <> freshIdent)
        ("int", _) | isFloatingType targetType -> do
            freshIdent <- newIdent ".local"
            emit (Instruction freshIdent (toQbeTy targetType) "swtof" [val])
            return ("%" <> freshIdent)
        ("uint", _) | isFloatingType targetType -> do
            freshIdent <- newIdent ".local"
            emit (Instruction freshIdent (toQbeTy targetType) "uwtof" [val])
            return ("%" <> freshIdent)
        ("long", _) | isFloatingType targetType -> do
            freshIdent <- newIdent ".local"
            emit (Instruction freshIdent (toQbeTy targetType) "sltof" [val])
            return ("%" <> freshIdent)
        ("ulong", _) | isFloatingType targetType -> do
            freshIdent <- newIdent ".local"
            emit (Instruction freshIdent (toQbeTy targetType) "ultof" [val])
            return ("%" <> freshIdent)
        ("float", "double") -> do
            freshIdent <- newIdent ".local"
            emit (Instruction freshIdent "d" "exts" [val])
            return ("%" <> freshIdent)
        ("double", "float") -> do
            freshIdent <- newIdent ".local"
            emit (Instruction freshIdent "s" "truncd" [val])
            return ("%" <> freshIdent)
        ("double", _) | isIntegerType targetType -> do
            freshIdent <- newIdent ".local"
            emit (Instruction freshIdent (toQbeTy targetType) "dtosi" [val])
            return ("%" <> freshIdent)
        ("float", _) | isIntegerType targetType -> do
            freshIdent <- newIdent ".local"
            emit (Instruction freshIdent (toQbeTy targetType) "stosi" [val])
            return ("%" <> freshIdent)
        (l, r) ->
            fail ("Conversion from " ++ show l ++ " to "
                ++ show r ++ " not implemented yet")
emitCast parameterType targetType _ =
    fail ("Conversion from " ++ show parameterType ++ " to "
        ++ show targetType ++ " not implemented yet")

isFloatingType :: Type -> Bool
isFloatingType ty =
    elem ty [doubleType, floatType]

isIntegerType :: Type -> Bool
isIntegerType ty =
    elem ty [charType, shortType, ushortType, intType, uintType, longType, ulongType]

literalToVal :: Literal -> Emit Val
literalToVal (StringLiteral str) = registerConstant str
literalToVal l = return (literalToText l)

literalToText :: Literal -> Text
literalToText (Int32 l) = pack (show l)
literalToText (UInt32 l) = pack (show l)
literalToText (Int64 l) = pack (show l)
literalToText (UInt64 l) = pack (show l)
literalToText (Float32 l) = "s_" <> pack (show l)
literalToText (Float64 l) = "d_" <> pack (show l)
literalToText (Bool True) = trueVal
literalToText (Bool False) = falseVal
literalToText (StringLiteral _) =
    error "String literals have to be handled with literalToVal"

falseVal :: Val
falseVal = "0"

trueVal :: Val
trueVal = "1"

pointerTy :: Ty
pointerTy = "l"

voidTy :: Ty
voidTy = ""

-- Handles sub-word types, used for data types and stores
-- sign does not matter
toQbeStoreTy :: Type -> Ty
toQbeStoreTy ty = case toQbeTy ty of
    "ub" -> "b"
    "sh" -> "h"
    "uh" -> "h"
    other -> other

-- Temporaries have to have at least word size
toQbeWordTy :: Type -> Ty
toQbeWordTy ty = case toQbeTy ty of
    "ub" -> "w"
    "sh" -> "w"
    "uh" -> "w"
    other -> other

-- Used for variables, smaller and unsigned types just fall back to w
toQbeTy :: Type -> Ty
toQbeTy (Concrete name []) =
    case getText name of
        "bool" -> "ub"
        "char" -> "ub"
        "short" -> "sh"
        "ushort" -> "uh"
        "int" -> "w"
        -- loadw can be used insted of loaduw because signed does not matter
        "uint" -> "w"
        "long" -> "l"
        "ulong" -> "l"
        "usize" -> "l"
        "float" -> "s"
        "double" -> "d"
        -- void type is just left empty for functions
        "void" -> voidTy
        "string" -> pointerTy
        "auto" -> error "Error: auto appeared in printing stage"
        s -> ":" <> s
toQbeTy (PointerType _) = pointerTy
-- TODO how to handle array types? always decay to pointer?
toQbeTy (ArrayType _ _) = pointerTy
toQbeTy (FunctionType _ _) = pointerTy
toQbeTy other =
    error ("Error: generic or function type " ++ show other ++ " appeared in printing stage")

newLabel :: Text -> Emit Text
newLabel = newIdent

registerConstant :: Text -> Emit Text
registerConstant str = do
    freshName <- newIdent "string"
    ref <- asks getStringLiterals
    let escaped = toEscapedNullString str
    lift (modifyIORef' ref (\c -> c ++ [DataDef freshName [("b", [escaped])]]))
    return ("$" <> freshName)

toEscapedNullString :: Text -> Text
toEscapedNullString str =
    "\"" <> escape str <> "\\0\""

-- Pretty Printing to Qbe format
moduleToQbe :: Module -> IO Mod
moduleToQbe (Module name statements) = do
    uniques <- newIORef 0
    stringDefs <- newIORef []
    instructions <- newIORef []
    allocs <- newIORef []
    let globalNames = gatherGlobalNames statements
    let structs = gatherStructs statements
    let baseEmitEnv = EmitEnv globalNames [] uniques stringDefs structs instructions allocs Nothing
    let typeDefs = foldMap structToDef statements
    defs <- runReaderT (traverse statementToDef statements) baseEmitEnv
    createdStringDefs <- readIORef stringDefs
    return (Mod (getText name) (typeDefs ++ createdStringDefs ++ concat defs))

-- Top level
statementToDef :: Statement -> Emit [Def]
statementToDef (Definition name e) = do
    items <- expressionToData e
    return [DataDef (getInnerText name) items]
statementToDef (FunctionDefinition name [] ty parameters statements) = do
    let qbeParams = fmap (\param -> (toQbeTy (getType param), getInnerText param)) parameters
    let returnType = toQbeTy ty
    blocks <- withFreshEmitEnv (bodyToBlock (fmap snd qbeParams) statements)
    -- ensures that a block for a function ends with a ret
    let blocks' = addRetIfMissing blocks
    return [FuncDef returnType (getText name) qbeParams blocks']
statementToDef (ExternDefinition _ _ _) = return []
statementToDef (StructDefinition _ _ _) = return []
statementToDef s =
    fail ("Unexpected statement at top level " ++ show s)

-- TODO handle expressions. The hard part is,
-- that this can emit defs, but not instructions
expressionToData :: Expression -> Emit [(Ty, [Val])]
expressionToData (Literal (StringLiteral str)) = do
    name <- registerConstant str
    return [(toQbeStoreTy stringType, [name])]
expressionToData e@(Literal l) =
    let item = literalToText l
    in return [(toQbeStoreTy (readType e), [item])]
-- TODO constructors, create array as w 1 2 3 instead of w 1, w 2, w 3
expressionToData (ArrayExpression es) =
    fmap concat (traverse expressionToData es)

-- TODO solve problem with void functions
-- Following function:
-- void main() { putchar(100); }
-- would compile fine, but the ret would return the 100
-- as error code.
-- We can't simply end with ret 0, because this would be a syntax error for
-- void functions.
addRetIfMissing :: [Block] -> [Block]
addRetIfMissing blocks =
    if endsWithRet blocks
        then blocks
        else blocks ++ [Ret Nothing]

endsWithRet :: [Block] -> Bool
endsWithRet blocks =
    not (null blocks) && isRet (last blocks)

isRet :: Block -> Bool
isRet (Ret _) = True
isRet _ = False

isJumpStatement :: Statement -> Bool
isJumpStatement (Return _) = True
isJumpStatement BreakStatement = True
isJumpStatement ContinueStatement = True
isJumpStatement _ = False

structToDef :: Statement -> [Def]
structToDef (StructDefinition ident [] fields) =
    [TypeDef (getText ident) (fmap (toQbeStoreTy . getType) fields)]
structToDef _ = []

gatherGlobalNames :: [Statement] -> [Text]
gatherGlobalNames = foldMap gatherGlobalName

gatherGlobalName :: Statement -> [Text]
gatherGlobalName (Definition name _) = [getInnerText name]
gatherGlobalName (FunctionDefinition name _ _ _ _) = [getText name]
gatherGlobalName (ExternDefinition name _ _) = [getText name]
gatherGlobalName _ = []

prettyMod :: Mod -> Doc ann
prettyMod (Mod name defs) =
    fromText "# Compiled from" <+> fromText name <> ".quo"
    <//> intercalate "\n\n" (fmap prettyDef defs)

-- For function definitions
prettyFunParam :: (Ty, Ident) -> Doc a
prettyFunParam (ty, ident) = prettyTy ty <+> "%" <> fromText ident

-- For calls
prettyParam :: (Ty, Val) -> Doc a
prettyParam (ty, val) = prettyTy ty <+> prettyVal val

-- For data definitions
prettyData :: (Ty, [Val]) -> Doc a
prettyData (ty, vals) = prettyTy ty <+> intercalate " " (fmap prettyVal vals)

prettyTy :: Ty -> Doc a
prettyTy ty = fromText ty

prettyVal :: Val -> Doc a
prettyVal val = fromText val

prettyDef :: Def -> Doc a
prettyDef (FuncDef ty ident parameters block) =
    "export function" <+> prettyTy ty <+> "$" <> fromText ident
    <> parens (intercalate ", " (fmap prettyFunParam parameters))
    <+> "{"
    <//> "@start"
    <//> intercalate "\n" (fmap prettyBlock block)
    <//> "}"
prettyDef (DataDef ident items) =
    "export data" <+> "$" <> fromText ident <+> "=" <+> "{"
        <> intercalate ", " (fmap prettyData items)
        <> "}"
prettyDef (TypeDef ident fields) =
    "type" <+> ":" <> fromText ident <+> "=" <+> "{"
        <> intercalate ", " (fmap prettyTy fields)
        <> "}"

prettyBlock :: Block -> Doc a
prettyBlock (Instruction ident ty instr parameters) =
    indent ("%" <> fromText ident <+> "=" <> prettyTy ty <+> fromText instr <+> intercalate ", " (fmap prettyVal parameters))
prettyBlock (CallInstruction ident ty f parameters) =
    indent ((if ty == voidTy
        then ""
        else "%" <> fromText ident <+> "=" <> prettyTy ty <+> "") <> "call" <+> fromText f <> parens (intercalate ", " (fmap prettyParam parameters)))
prettyBlock (Store ty val addressVal) =
    indent ("store" <> fromText ty <+> prettyVal val <> "," <+> prettyVal addressVal)
prettyBlock (Label label) =
    -- Label is not indented
    "@" <> fromText label
prettyBlock (Jump label) =
    indent ("jmp" <+> "@" <> fromText label)
prettyBlock (JumpNonZero val thenLabel elseLabel) =
    indent ("jnz" <+> prettyVal val <> "," <+> "@" <> fromText thenLabel <> "," <+> "@" <> fromText elseLabel)
prettyBlock (Ret (Just val)) =
    indent ("ret" <+> prettyVal val)
prettyBlock (Ret Nothing) =
    indent "ret"
