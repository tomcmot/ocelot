module Run where
import Prelude hiding (lex)
import Control.Monad.State
import Data.List (dropWhileEnd)

import Eval
import Lexer (lex, Token(Indent))
import Parser (parse)

run :: FilePath -> IO ()
run file = do
    contents <- readFile file
    evalStateT (rep contents) []

rep :: String -> Rep ()
rep s = do
    let tokens = lex s
    let ast = parse (dropWhileEnd (== Indent 0) tokens)
    result <- eval ast
    lift $ print result