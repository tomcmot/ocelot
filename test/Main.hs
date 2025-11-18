module Main (main) where

import Test.Hspec
import Control.Monad (forM_)
import Lexer qualified

main :: IO ()
main = hspec $ do
    lexerSpec

lexerSpec :: SpecWith ()
lexerSpec = describe "Lexer" $ do
    let examples = 
            [ ("1 + 2", [Lexer.Number 1, Lexer.Identifier "+", Lexer.Number 2])
            , ("let x =\n  1\nx", [Lexer.Identifier "let", Lexer.Identifier "x", Lexer.Identifier "=", Lexer.Indent 2,Lexer.Number 1, Lexer.Indent 0,  Lexer.Identifier "x"])
            , ("let x = 1\nlet y = 2", [Lexer.Identifier "let", Lexer.Identifier "x", Lexer.Identifier "=", Lexer.Number 1, Lexer.Indent 0,Lexer.Identifier "let", Lexer.Identifier "y", Lexer.Identifier "=", Lexer.Number 2])
            ]
    forM_ examples $ \(ex, expected) -> it (show ex) $ do
        let tokens = Lexer.lex ex
        tokens `shouldBe` expected