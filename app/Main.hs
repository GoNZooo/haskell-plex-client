{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Options.Applicative.Simple
import qualified Paths_plex_client
import Plex.Types
import Qtility
import Run

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_plex_client.version)
      "Header for command line arguments"
      "Program description, also for command line arguments"
      ( Options
          <$> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
            )
          <*> strOption
            ( long "environment-file"
                <> short 'e'
                <> metavar "DOTENV_FILE"
                <> help "Environment file to load during startup"
                <> value ".env"
            )
      )
      empty
  run options
