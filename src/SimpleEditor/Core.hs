module SimpleEditor.Core where

import Control.Monad ( forM_ )

import SimpleEditor.Parse
import SimpleEditor.Process
import SimpleEditor.Shell

startApplication
  :: Shell m
  => m ()
startApplication = do
  commands <- parseSTDIN
  let results = processCommands commands
  forM_ results $ putLn . (: [])

