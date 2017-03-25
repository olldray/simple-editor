module SimpleEditor.Parse
  ( parseSTDIN
  , parseCommand
  ) where

import Control.Monad ( replicateM )

import SimpleEditor.Shell
import SimpleEditor.Types

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

