{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FakeShell
  ( FakeShell (..)
  , FakeShellState (..)
  ) where

import SimpleEditor.Shell
import Control.Monad.State.Lazy ( State (..)
                                , gets
                                , modify
                                )

data FakeShellState = FakeShellState
  { fssSTDIN :: [String]
  , fssSTDOUT :: [String]
  }
  deriving (Show, Eq)

newtype FakeShell a = FakeShell (State FakeShellState a)
  deriving (Monad, Applicative, Functor)

instance Shell FakeShell where
  getLn = FakeShell $ do
    (x:xs) <- gets fssSTDIN
    modify $ \s -> s {fssSTDIN = xs}
    return x
  putLn str = FakeShell $ do
    xs <- gets fssSTDOUT
    modify $ \s -> s {fssSTDOUT = str:xs}
