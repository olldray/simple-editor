module SimpleEditor.Core where

import Control.Monad ( forM_
                     , replicateM
                     )
import SimpleEditor.Shell

startApplication
  :: Shell m
  => m ()
startApplication = do
  commands <- parseSTDIN
  let results = processCommands commands
  forM_ results $ putLn . (: [])


-- These parsing functions are filled with partials.
-- The only reason this is even slightly
-- acceptable is becuase the problem statement
-- swore up and down that we would never get
-- bad input.
parseSTDIN
  :: Shell m
  => m [Command]
parseSTDIN = do
  numCommands <- read <$> getLn
  replicateM numCommands $ parseCommand <$> getLn

parseCommand
  :: String
  -> Command
parseCommand str =
  let
    cmd = read $ takeWhile (/= ' ') str :: Int
    argStr = dropWhile (== ' ') $ dropWhile (/= ' ') str
  in
    case cmd of
      1 -> DoAction (Append $ argStr)
      2 -> DoAction (Delete $ read argStr)
      3 -> Print $ read argStr
      _ -> Undo -- Ideally this would be '4' and we would have
                -- a seperate "unexpected command" case

processCommands
  :: [Command]
  -> [Char]
processCommands commands =
  let
    finalState = foldr processCommand emptyState commands
  in
    reverse $ psOutput finalState

processCommand
  :: Command
  -> ProcessingState
  -> ProcessingState
processCommand cmd state =
  case cmd of
    DoAction action -> processAction action state
    Print k -> processPrint k state
    Undo -> processUndo state

processAction
  :: Action
  -> ProcessingState
  -> ProcessingState
processAction = flip const

processPrint
  :: Int
  -> ProcessingState
  -> ProcessingState
processPrint = flip const

processUndo
  :: ProcessingState
  -> ProcessingState
processUndo = id

data ProcessingState = ProcessingState
  { psS :: [Char]
  , psUndo :: [Action]
  , psOutput :: [Char]
  }

emptyState :: ProcessingState
emptyState = ProcessingState
                { psS = []
                , psUndo = []
                , psOutput = []
                }

data Command =
    DoAction Action
  | Print Int
  | Undo
  deriving (Show, Eq)

data Action =
    Append String
  | Delete Int
  deriving (Show, Eq)
