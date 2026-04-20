module Main where
import System.Environment
import Control.Monad (forM_)

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \ arg ->
    putStrLn arg
  putStrLn "hello"
