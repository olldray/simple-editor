module SimpleEditor.Process
  ( processCommands
  , processAction
  , processPrint
  , processUndo
  ) where

import SimpleEditor.Types

processCommands
  :: [Command]
  -> [Char]
processCommands commands =
  let
    finalState = foldl processCommand emptyState commands
  in
    reverse $ psOutput finalState

processCommand
  :: ProcessingState
  -> Command
  -> ProcessingState
processCommand state cmd =
  case cmd of
    DoAction action -> processAction action state
    Print k -> processPrint k state
    Undo -> processUndo state

processAction
  :: Action
  -> ProcessingState
  -> ProcessingState
processAction (Append str) state =
  let
    newS = foldl (flip (:)) (psS state) str
    newL = length str + psLength state
    newUndo = (Delete $ length str) : psUndo state
  in
    state { psS = newS, psLength = newL, psUndo = newUndo }
processAction (Delete k) state =
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
    newOut = (psS state !! i) : psOutput state -- assume k was valid
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

