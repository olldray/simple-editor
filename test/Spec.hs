module Main where

import Control.Monad.State.Lazy ( execState
                                , evalState
                                )
import Data.Monoid ( (<>) )

import FakeShell
import SimpleEditor.Core
import SimpleEditor.Parse
import SimpleEditor.Process
import SimpleEditor.Types
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Simple Editor" $ do
    it "append and print one character" $ do
      let result = runApplication oneChar
      result `shouldBe` oneCharResult

    it "enormous test" $ do
      let result = runApplication enormous
      result `shouldBe` enormousResult

  describe "parseSTDIN" $ do
    it "parse series of commands" $ do
      let result = evalState (unFake parseSTDIN) oneChar
      result `shouldBe` [DoAction (Append "a"), Print 1]

  describe "process commands" $ do
    it "process a few" $ do
      let result = processCommands oneOfEach
      result `shouldBe` ['s', 'q', 'w']

  describe "process individual command" $ do
    it "process an append" $ do
      let final = processAction (Append "bar") $ ProcessingState "of" 2 [Delete 2] []
      final `shouldBe` ProcessingState "rabof" 5 [Delete 3, Delete 2] []

    it "process a delete" $ do
      let final = processAction (Delete 4) $ ProcessingState "raboof" 6 [Delete 3, Delete 3] []
      final `shouldBe` ProcessingState "of" 2 [Append "obar", Delete 3, Delete 3] []

    it "process a print" $ do
      let final = processPrint 4 $ ProcessingState "raboof" 6 [Delete 3, Delete 3] []
      final `shouldBe` ProcessingState "raboof" 6 [Delete 3, Delete 3] ['b']

    it "process an undo append" $ do
      let final = processUndo $ ProcessingState "raboof" 6 [Delete 3, Delete 3] []
      final `shouldBe` ProcessingState "oof" 3 [Delete 3] []

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

bigStr = take 999999 $ cycle "areallylongstring"
enorInput = concat [ [ "1000000"
                     , "1 " <> bigStr ]
                   , replicate 499999 "2 1"
                   , replicate 499999 "4"
                   , [ "3 1" ]
                   ]
enormous = FakeShellState { fssSTDIN = enorInput, fssSTDOUT = [] }
enormousResult = FakeShellState { fssSTDIN = [], fssSTDOUT = ["a"] }

oneOfEach = [ DoAction (Append "wow")
            , DoAction (Append "sers")
            , Print 4
            , DoAction (Delete 6)
            , DoAction (Append "nquy")
            , Print 3
            , Undo
            , Undo
            , Undo
            , Print 3
            ]

runApplication :: FakeShellState -> FakeShellState
runApplication =
  let (FakeShell action) = startApplication
  in execState action
