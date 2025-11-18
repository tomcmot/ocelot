module Typed where
import Surface
data Type
    = TCon String
    deriving (Eq, Show)

check :: [(String, Type)] -> Expr -> Either String Type
check env = \case
    Var s -> case lookup s env of
        Just t -> Right t
        Nothing -> Left ("undefined variable: " ++ s)
    Int _ -> Right (TCon "Int")
    Add e1 e2 -> binary (TCon "Int") e1 e2
    Mul e1 e2 -> binary (TCon "Int") e1 e2
    Sub e1 e2 -> binary (TCon "Int") e1 e2
    Neg e -> unary (TCon "Int") e
    Bool _ -> Right (TCon "Bool")
    And e1 e2 -> binary (TCon "Bool") e1 e2
    Or e1 e2 -> binary (TCon "Bool") e1 e2
    Not e -> unary (TCon "Bool") e
    where
        unary t a = do
            at <- check env a
            if t == at
                then Right t
                else Left "type mismatch"
        binary t a b = do
            at <- check env a
            bt <- check env b
            if t == at && t == bt
                then Right t
                else Left "type mismatch"

    