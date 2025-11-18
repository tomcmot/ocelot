module Main where
import System.Environment
import Repl
import Run
main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> run file
    _ -> repl
