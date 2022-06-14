{-# LANGUAGE TemplateHaskell #-}

module Types where

import Qtility
import RIO.Process (HasProcessContext (..), ProcessContext)

newtype XmlDecodingError = XmlDecodingError {unXmlDecodingError :: SomeException}
  deriving (Show)

instance Exception XmlDecodingError

class PlexRequest request response | request -> response where
  executeRequest :: PlexIp -> PlexToken -> request -> IO response

newtype PlexToken = PlexToken {unPlexToken :: String}
  deriving (Eq, Show, Read, IsString, Generic, FromEnvironmentValue)

class HasPlexToken env where
  plexTokenL :: Lens' env PlexToken

instance HasPlexToken PlexToken where
  plexTokenL = id

newtype PlexIp = PlexIp {unPlexIp :: String}
  deriving (Eq, Show, Read, IsString, Generic, FromEnvironmentValue)

class HasPlexIp env where
  plexIpL :: Lens' env PlexIp

instance HasPlexIp PlexIp where
  plexIpL = id

newtype PlexClientIdentifier = PlexClientIdentifier {unPlexClientIdentifier :: Text}
  deriving (Eq, Show, Read, IsString, Generic)

-- | Command line arguments
data Options = Options
  { _optionsVerbose :: !Bool,
    _optionsEnvironmentFile :: !EnvironmentFile
  }

data App = App
  { _appLogFunc :: !LogFunc,
    _appProcessContext :: !ProcessContext,
    _appOptions :: !Options,
    _appPlexToken :: !PlexToken,
    _appPlexIp :: !PlexIp
  }

data GetDevicesRequest = GetDevicesRequest

newtype GetDevicesResponse = GetDevicesResponse {unGetDevicesResponse :: [PlexDevice]}
  deriving (Eq, Show)

data PlexDevice = PlexDevice
  { _plexDeviceId :: !Integer,
    _plexDeviceName :: !String,
    _plexDeviceClientIdentifier :: !PlexClientIdentifier
  }
  deriving (Eq, Show)

instance PlexRequest GetDevicesRequest GetDevicesResponse where
  executeRequest _ip _token _request = undefined

foldMapM makeLenses [''Options, ''App]

foldMapM makeWrapped [''PlexToken, ''PlexIp, ''XmlDecodingError]

instance HasLogFunc App where
  logFuncL = appLogFunc

instance HasProcessContext App where
  processContextL = appProcessContext

instance HasPlexToken App where
  plexTokenL = appPlexToken

instance HasPlexIp App where
  plexIpL = appPlexIp
