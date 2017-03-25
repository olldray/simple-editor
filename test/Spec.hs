module Main where

import Control.Monad.State.Lazy ( execState
                                , evalState
                                )
import FakeShell
import SimpleEditor.Core
import Test.Hspec
import Test.QuickCheck


main :: IO ()
main = hspec $ do
  describe "Simple Editor" $ do
    it "append and print one character" $ do
      let result = runApplication oneChar
      result `shouldBe` oneCharResult

  describe "parseSTDIN" $ do
    it "parse series of commands" $ do
      let result = evalState (unFake parseSTDIN) oneChar
      result `shouldBe` [DoAction (Append "a"), Print 1]

  describe "process commands" $ do
    it "process one of each" $ do
      let result = processCommands oneOfEach
      result `shouldBe` ['w']

  describe "parseCommand" $ do
    it "parse append" $ do
      parseCommand "1 abc" `shouldBe` DoAction (Append "abc")

    it "parse delete" $ do
      parseCommand "2 623" `shouldBe` DoAction (Delete 623)

    it "parse print" $ do
      parseCommand "3 4231" `shouldBe` Print 4231

    it "parse undo" $ do
      parseCommand "4" `shouldBe` Undo

oneChar = FakeShellState { fssSTDIN = ["2", "1 a", "3 1"], fssSTDOUT = [] }
oneCharResult = FakeShellState { fssSTDIN = [], fssSTDOUT = ["a"] }

oneOfEach = [DoAction (Append "wowsers"), DoAction (Delete 6), Undo, Print 3]

runApplication :: FakeShellState -> FakeShellState
runApplication =
  let (FakeShell action) = startApplication
  in execState action
