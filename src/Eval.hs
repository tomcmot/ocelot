module Eval where

import Control.Monad.State
import Surface
data Value
    = VInt Int
    | VClosure
instance Show Value where
    show (VInt i) = show i
    show (VClosure) = "<function>"

type Env = [(String, Value)]

type Rep = StateT Env IO

eval :: Expr -> Rep (Either String Value)
eval = \case
    Int i -> return (Right (VInt i))
    Var s -> do
        env <- get
        case lookup s env of
            Just v -> return (Right v)
            Nothing -> return (Left ("undefined variable: " ++ s))
    _ -> undefined