{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parser where
import Data.Set qualified as Set
import Text.Megaparsec
import Surface qualified as S
import Lexer qualified as L
import Data.Void (Void)
import Data.List.NonEmpty (NonEmpty((:|)))
import Control.Monad (when)

type Parser = Parsec Void [L.Token]

number :: Parser Int
number = labeled (\case L.Number n -> Just n; _ -> Nothing) "number"

keyword :: String -> Parser ()
keyword s = labeled (\case L.Identifier s' | s == s' -> Just (); _ -> Nothing) s
keyOp :: String -> Parser ()
keyOp o = labeled (\case L.Operator o' | o == o' -> Just (); _ -> Nothing) o

variable :: Parser String
variable = labeled (\case L.Identifier s -> Just s; _ -> Nothing) "variable"

operator :: Parser String
operator = labeled (\case L.Operator s -> Just s; _ -> Nothing) "operator"

lparen, rparen, lbrace, rbrace, lbracket, rbracket, comma, semicolon :: Parser ()
lparen = labeled (\case L.LParen -> Just (); _ -> Nothing) "("
rparen = labeled (\case L.RParen -> Just (); _ -> Nothing) ")"
lbrace = labeled (\case L.LBrace -> Just (); _ -> Nothing) "{"
rbrace = labeled (\case L.RBrace -> Just (); _ -> Nothing) "}"
lbracket = labeled (\case L.LBracket -> Just (); _ -> Nothing) "["
rbracket = labeled (\case L.RBracket -> Just (); _ -> Nothing) "]"
comma = labeled (\case L.Comma -> Just (); _ -> Nothing) ","
semicolon = labeled (\case L.SemiColon -> Just (); _ -> Nothing) ";"

labeling :: [Char] -> ErrorItem t
labeling (c:r) = Label (c:|r)
labeling _ = undefined

labeled :: (L.Token  -> Maybe a) -> String -> Parser a
labeled p s = token p (Set.singleton (labeling s))

indentation :: Parser Int
indentation = labeled (\case L.Indent i -> Just i; _ -> Nothing) "indentation"

guarded :: Int -> Parser ()
guarded i = do
    l <- some indentation
    let j = last l
    when (i == j) $ fail ""
    pure ()

parseStmt :: Parser S.Stmt
parseStmt = do
    level <- indentation
    xs <- someTill (S.Expr <$> parseExpr <|> parseLet) (guarded level)
    case xs of
        [x] -> return x
        _ -> return $ S.Seq xs
    where
        parseLet = do
            keyword "let"
            x <- variable
            keyOp "="
            y <- parseStmt
            return $ S.Let x y

parseExpr :: Parser S.Expr
parseExpr = do
    a <- simple
    r <- optional $ do
        o <- operator
        b <- parseExpr
        pure (o, b)
    case r of 
        Nothing -> return a
        Just (o, b) -> return $ S.Binary o a b
    where 
        simple =
            S.Bool <$> ((keyword "true" >> pure True) <|> (keyword "false" >> pure False)) <|>
            S.Int <$> number <|>
            S.Var <$> variable <|>
            S.Unary <$> operator <*> simple <|>
            between lparen rparen parseExpr

parse = undefined