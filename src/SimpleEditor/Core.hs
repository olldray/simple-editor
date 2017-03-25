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
    finalState = foldl (flip processCommand) emptyState commands
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
processAction action state = applyAction action state

applyAction
  :: Action
  -> ProcessingState
  -> ProcessingState
applyAction (Append str) state =
  let
    newS = foldl (flip (:)) (psS state) str
    newL = length str + psLength state
    newUndo = (Delete $ length str) : psUndo state
  in
    state { psS = newS, psLength = newL, psUndo = newUndo }
applyAction (Delete k) state =
  let
    (deleted, remainder) = splitAt k $ psS state
    newL = psLength state - k
    newUndo = (Append $ reverse deleted) : psUndo state
  in
    state { psS = remainder, psLength = newL, psUndo = newUndo }

processPrint
  :: Int
  -> ProcessingState
  -> ProcessingState
processPrint k state =
  let
    i = psLength state - k
    newOut = (psS state !! i) : psOutput state
  in
    state { psOutput = newOut }

processUndo
  :: ProcessingState
  -> ProcessingState
processUndo state =
  let
    (action:rest) = psUndo state -- irrefutable pattern match
    newState = processAction action state
  in
    newState { psUndo = rest }

data ProcessingState = ProcessingState
  { psS :: [Char]
  , psLength :: Int
  , psUndo :: [Action]
  , psOutput :: [Char]
  }
  deriving (Show, Eq)

emptyState :: ProcessingState
emptyState = ProcessingState
                { psS = []
                , psLength = 0
                , psUndo = []
                , psOutput = []
                }

data Command =
    DoAction Action
  | Print Int
  | Undo
  deriving (Show, Eq)

data Action =
    Append [Char]
  | Delete Int
  deriving (Show, Eq)
