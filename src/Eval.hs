module Eval where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except
import Data.Map (Map)
import Data.Map qualified as Map

data Value
    = Int Int
    | Var String
    | Thunk Comp

data Comp
    = Return Value
    | Force Value
    | Bind Comp (Value -> Eval Comp)
    | Lambda (Value -> Eval Comp)
    | Apply Comp Value

instance Eq Value where
    Int x == Int y = x == y
    _ == _ = False

instance Show Value where
    show (Int i) = show i
    show (Var x) = x
    show (Thunk _) = "<thunk>"

type Env = Map String Value

type Eval a = ExceptT String IO a

prelude :: Env
prelude = Map.fromList 
    [ ("+", Thunk . Lambda $ binaryIntOp (+))
    , ("-", Thunk . Lambda $ binaryIntOp (-))
    , ("*", Thunk . Lambda $ binaryIntOp (*))
    , ("/", Thunk . Lambda $ safeDivide)
    , ("print", Thunk . Lambda $ \ v -> do
        liftIO $ print v
        pure . Return $ Int 0)
    ]
    where 
        binaryIntOp op = binary (\ x y -> return . Return . Int $ op x y)
        binary op = \case
            Int x -> return $ Lambda $ \case
                Int y -> op x y
                _ -> throwE "type error: expected int" -- todo track locations for errors
            _ -> throwE "type error: expected int"
        safeDivide = binary (\ x y -> if y == 0 then throwE "division by zero" else return . Return . Int $ (x `div` y))
            

resolve :: Env -> Value -> Eval Value
resolve env = \case
    Var x -> 
        case Map.lookup x env of
            Just v -> pure v
            Nothing -> throwE ("unbound variable: " ++ x)
    e -> pure e

extract :: Env -> Comp -> Eval (Value -> Eval Comp)
extract env = \case
    Lambda c -> pure c
    Force v -> resolveThunk v
    c -> eval env c >>= resolveThunk -- probably too lenient 
    where
        resolveThunk v = do
            v' <- resolve env v
            case v' of
                Thunk c -> extract env c
                _ -> throwE "cannot apply integer"

eval :: Env -> Comp -> Eval Value
eval env = \case
    Return i -> resolve env i
    Force v -> do
        v' <- resolve env v 
        case v' of
            (Thunk c) -> eval env c
            _ -> throwE "can only force a thunk"
    Bind c rest -> do
        v <- eval env c
        k <- rest v
        eval env k
    Lambda c -> pure . Thunk . Lambda $ c
    Apply f a -> do 
        f' <- extract env f
        k <- f' a
        eval env k