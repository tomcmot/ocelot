module Repl (repl) where

import Prelude hiding (lex)

import Control.Monad.State
import Lexer (lex, isComplete)
import Parser (parse)
import Eval (eval, Rep)
import System.IO (hFlush, stdout)


repl :: IO ()
repl = runStateT loop [] >> return ()

loop :: Rep ()
loop = do
    lift $ putStr "> "
    lift $ hFlush stdout
    line <- lift getLine
    multiline line

multiline :: String -> Rep ()
multiline line = do
    let tokens = lex line
    lift $ print tokens 
    lift $ hFlush stdout
    if isComplete tokens
        then do
            let ast = parse (init tokens)
            result <- eval ast
            lift $ print result
            lift $ hFlush stdout
            loop
        else do
            next <- lift getLine
            lift $ hFlush stdout
            multiline (line ++ "\n" ++ next)