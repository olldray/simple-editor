module SimpleEditor.Core where

import SimpleEditor.Shell

startApplication
  :: Shell m
  => m ()
startApplication = do
  f <- getLn
  putLn f
