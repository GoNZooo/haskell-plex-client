module Run (run) where

import Plex.Types
import Qtility
import RIO.Process (mkDefaultProcessContext)

run :: Options -> IO ()
run options = do
  loadDotEnvFile $ options ^. optionsEnvironmentFile
  lo <- logOptionsHandle stderr (options ^. optionsVerbose)
  pc <- mkDefaultProcessContext
  plexToken <- readEnvironmentVariable "PLEX_TOKEN"
  plexIp <- readEnvironmentVariable "PLEX_IP"
  withLogFunc lo $ \lf ->
    let app =
          App
            { _appLogFunc = lf,
              _appProcessContext = pc,
              _appOptions = options,
              _appPlexToken = plexToken,
              _appPlexIp = plexIp
            }
     in runRIO app runApp

runApp :: RIO App ()
runApp = do
  logInfo "We're inside the application!"
