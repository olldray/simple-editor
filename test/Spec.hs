module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.State.Lazy ( execState )
import FakeShell
import SimpleEditor.Core


main :: IO ()
main = hspec $ do
  describe "Simple Editor" $ do
    it "append and print one character" $ do
      let result = runApplication oneChar
      result `shouldBe` oneCharResult

oneChar = FakeShellState { fssSTDIN = ["2", "1 a", "3 1"], fssSTDOUT = [] }
oneCharResult = FakeShellState { fssSTDIN = [], fssSTDOUT = ["a"] }

runApplication :: FakeShellState -> FakeShellState
runApplication =
  let (FakeShell action) = startApplication
  in execState action
