module SimpleEditor.Types
  ( Command (..)
  , Action (..)
  , ProcessingState (..)
  , emptyState
  ) where


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
