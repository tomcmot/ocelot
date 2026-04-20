module Main (main) where

import Test.Hspec
import Control.Monad.Trans.Except
import Eval qualified

main :: IO ()
main = hspec $ do
    describe "Eval" $ do
        it "simple addition" $ do
            result <- runExceptT $ 
                Eval.eval 
                    Eval.prelude 
                    ( Eval.Bind 
                        ( Eval.Apply 
                            (Eval.Force (Eval.Var "+")) 
                            (Eval.Int 1)
                        )
                        ( \ f -> pure $ Eval.Apply (Eval.Force f) (Eval.Int 2)
                        )
                    )
            result `shouldBe` Right (Eval.Int 3)
        it "identity returns arg" $ do
            result <- runExceptT $
                Eval.eval
                    Eval.prelude
                    (Eval.Bind
                        (Eval.Lambda (pure . Eval.Return))
                        (\ idV -> pure $ Eval.Apply (Eval.Force idV) (Eval.Int 0))
                    )
            result `shouldBe` Right (Eval.Int 0)