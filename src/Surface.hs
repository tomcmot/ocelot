module Surface where

data Expr
    = Var String
    | Int Int
    | Bool Bool
    | Unary String Expr
    | Binary String Expr Expr
    deriving (Eq, Show)

data Stmt
    = Expr Expr
    | Let String Stmt
    | Seq [Stmt]
    deriving (Eq, Show)

data Source 
    = Script
    | Module
    | Data
    deriving (Eq, Show)

sourceKind :: [Stmt] -> Source
sourceKind stmts =
    if all (\case Let _ _ -> True; _ -> False) stmts
    then Module
    else Data
