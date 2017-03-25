module SimpleEditor.Shell
  ( Shell (..)
  ) where

class Monad m => Shell m where
  getLn :: m String
  putLn :: String -> m ()

instance Shell IO where
  getLn = getLine
  putLn = putStrLn
