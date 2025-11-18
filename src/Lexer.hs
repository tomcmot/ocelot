module Lexer where
import Data.Char (isSpace, isNumber, isAlphaNum)
import Data.Maybe (catMaybes)
import Data.List.NonEmpty (nonEmpty, NonEmpty(..))
import Text.Read (readMaybe)

data Token
    = Indent Int
    | Number Int
    | Identifier String
    | LBrace | RBrace
    | LBracket | RBracket
    | LParen | RParen
    | Comma | SemiColon
    | Operator String
    deriving (Eq, Show, Ord)

lex :: String -> [Token]
lex input =  catMaybes $ getTokens (nonEmpty input)
    where 
        getTokens Nothing = []
        getTokens (Just s) = 
            let (token, rest) = classify s
            in token : getTokens (nonEmpty rest)

classify :: NonEmpty Char -> (Maybe Token, String)
classify (c:|r)
    | c == '\n' = 
        let (ws, r') = span isSpace r
        in (Just (Indent (length ws)), r')
    | c == '(' = (Just LParen, r)
    | c == ')' = (Just RParen, r)
    | c == '[' = (Just LBracket, r)
    | c == ']' = (Just RBracket, r)
    | c == '{' = (Just LBrace, r)
    | c == '}' = (Just RBrace, r)
    | c == ',' = (Just Comma, r)
    | c == ';' = (Just SemiColon, r)
    | isSpace c = (Nothing, r)
    | isNumber c = 
        let (num, r') = span isNumber r
        in (Number <$> readMaybe (c:num), r')
    | isOperator c =
        let (op, r') = span isOperator r
        in (Just (Operator (c:op)), r')
    | otherwise = 
        let (ident, r') = span isAlphaNum r
        in (Just (Identifier (c:ident)), r')

isComplete :: [Token] -> Bool
isComplete tokens = last tokens == Indent 0

isOperator :: Char -> Bool
isOperator c = c `elem` ("~!@#$%^&*-=+|/:<>.?" :: String)