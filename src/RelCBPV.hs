module RelCBPV where

import Control.Monad
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Bifunctor (second)

data Kind
    = ValType
    | CompType
    | KindArrow Kind Kind
    deriving (Eq, Show)

data Type
    = TyCon String
    | TyLam String Kind Type
    | TyApply Type Type
    | TyThnk Type -- must be computation type
    | TyUnit
    | TyPair Type Type
    | TySum [(String, Type)]
    | TyExist String Kind Type
    | TyRet Type
    | TyArrow Type Type
    | TyProduct [(String, Type)]
    | TyForall String Kind Type
    | TyFix String Type
    deriving (Eq, Show)

data Value
    = Var String
    | Thunk Comp
    | Unit
    | Pair Value Value
    | Tag String Value
    | Pack Type Value String Kind Type


data Comp
    = Force Value
    | Return Value
    | UnPair String String Value Comp
    | UnPack String String Value Comp
    | Match Value [(String, String, Comp)]
    | Bind String Comp Comp
    | Lambda String Type Comp
    | Apply Comp Value
    | CoMatch [(String, Comp)]
    | Select Comp String -- destruct comatch
    | ForAll String Kind Comp
    | ApplyTy Comp Type
    | Roll Comp
    | Unroll Comp
    | Fix String Comp

type KEnv = Map String Kind
type TyEnv = Map String Type


kindOf :: KEnv -> Type -> Maybe Kind
kindOf kenv = \case
    TyCon c ->
        Map.lookup c kenv
    TyLam x k b -> do
        bk <- kindOf (Map.insert x k kenv) b
        pure (KindArrow k bk)
    TyApply f a -> do
        fk <- kindOf kenv f
        ak <- kindOf kenv a
        case fk of
            KindArrow k r -> do
                guard (k == ak)
                pure r
            _ -> Nothing
    TyThnk t -> do
        tk <- kindOf kenv t
        guard (tk == CompType)
        pure ValType
    TyUnit -> pure ValType
    TyPair a b -> do
        ak <- kindOf kenv a
        bk <- kindOf kenv b
        guard (ak == ValType && bk == ValType)
        pure ValType
    TySum fields -> do
        fieldK <- mapM (kindOf kenv . snd) fields
        guard (all (== ValType) fieldK)
        pure ValType
    TyExist x k t -> do
        tk <- kindOf (Map.insert x k kenv) t
        guard (tk == ValType)
        pure ValType
    TyRet v -> do
        vk <- kindOf kenv v
        guard (vk == ValType)
        pure ValType
    TyArrow v r -> do
        vk <- kindOf kenv v
        rk <- kindOf kenv r
        guard (vk == ValType && rk == CompType)
        pure CompType
    TyProduct fields -> do
        fieldK <- mapM (kindOf kenv . snd) fields
        guard (all (== CompType) fieldK)
        pure CompType
    TyForall x k t -> do
        tk <- kindOf (Map.insert x k kenv) t
        guard (tk == CompType)
        pure CompType
    TyFix x b -> do
        bk <- kindOf (Map.insert x CompType kenv) b
        guard (bk == CompType)
        pure CompType

checkKind :: KEnv -> Kind -> Type -> Maybe ()
checkKind kenv k t = do
    tk <- kindOf kenv t
    guard (k == tk)


substType :: String -> Type -> Type -> Type
substType x witness = \case
    TyCon y | x == y -> witness
    t@(TyCon _) -> t
    TyUnit -> TyUnit

    TyExist y k t ->
        if x == y then TyExist y k t
        else TyExist y k (substType x witness t)
    TyLam y k t ->
        if x == y then TyLam y k t
        else TyLam y k (substType x witness t)
    TyForall y k t ->
        if x == y then TyForall y k t
        else TyForall y k (substType x witness t)
    TyFix y t ->
        if x == y then TyFix y t
        else TyFix y (substType x witness t)

    TyApply t1 t2 -> TyApply (substType x witness t1) (substType x witness t2)
    TyThnk t -> TyThnk (substType x witness t)
    TyPair t1 t2 -> TyPair (substType x witness t1) (substType x witness t2)
    TyRet t -> TyRet (substType x witness t)
    TyArrow t1 t2 -> TyArrow (substType x witness t1) (substType x witness t2)

    TySum fields -> TySum (map (second (substType x witness)) fields)
    TyProduct fields -> TyProduct (map (second (substType x witness)) fields)

typeOfValue :: KEnv -> TyEnv -> Value -> Maybe Type
typeOfValue kenv tenv = \case
    Var x -> Map.lookup x tenv
    Thunk c -> TyThnk <$> typeOfComp kenv tenv c
    Unit -> pure TyUnit
    Pair a b -> TyPair
        <$> typeOfValue kenv tenv a
        <*> typeOfValue kenv tenv b
    Tag c v -> TySum . (:[]) . (c,)
        <$> typeOfValue kenv tenv v
    Pack t v x k a -> do
        vt <- typeOfValue kenv tenv v
        let a' = substType x t a
        guard (vt == a')
        pure $ TyExist x k a


typeOfComp :: KEnv -> TyEnv -> Comp -> Maybe Type
typeOfComp kenv tenv = \case
    Return v -> TyRet <$> typeOfValue kenv tenv v
    Force v -> do
        vt <- typeOfValue kenv tenv v
        case vt of
            TyThnk t -> pure t
            _ -> Nothing
    Match _ _ -> undefined
    UnPair _ _ _ _ -> undefined
    UnPack _ _ _ _ -> undefined
    Lambda _ _ _ -> undefined
    Bind _ _ _ -> undefined
    Apply _ _ -> undefined
    CoMatch _ -> undefined
    ForAll _ _ _ -> undefined
    Select _ _ -> undefined
    ApplyTy _ _ -> undefined
    Roll _ -> undefined
    Unroll _ -> undefined
    Fix _ _ -> undefined