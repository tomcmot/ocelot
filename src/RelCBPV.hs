module RelCBPV where

import Control.Monad
import Data.Map (Map)
import Data.Map qualified as Map

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


substType :: String -> Type -> Type -> Type
substType x witness = \case
    TyCon y | x == y -> witness
    t@(TyCon _) -> t
    TyUnit -> TyUnit
    t -> t


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
    Pack t v x k a -> undefined

typeOfComp :: KEnv -> TyEnv -> Comp -> Maybe Type
typeOfComp kenv tenv = \case
    _ -> undefined