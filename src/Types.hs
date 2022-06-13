{-# LANGUAGE TemplateHaskell #-}

module Types where

import Qtility
import RIO.Process (HasProcessContext (..), ProcessContext)

-- | Command line arguments
data Options = Options
  { _optionsVerbose :: !Bool,
    _optionsEnvironmentFile :: !EnvironmentFile
  }

data App = App
  { _appLogFunc :: !LogFunc,
    _appProcessContext :: !ProcessContext,
    _appOptions :: !Options
  }

foldMapM makeLenses [''Options, ''App]

instance HasLogFunc App where
  logFuncL = appLogFunc

instance HasProcessContext App where
  processContextL = appProcessContext
