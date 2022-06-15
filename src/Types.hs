{-# LANGUAGE TemplateHaskell #-}

module Types where

import Qtility
import RIO.Process (HasProcessContext (..), ProcessContext)
import Text.XML (Document, Name)

newtype XmlDecodingError = XmlDecodingError {unXmlDecodingError :: SomeException}
  deriving (Show)

instance Exception XmlDecodingError

data XmlAttributeDecodingError = XmlAttributeDecodingError
  { _xmlAttributeDecodingErrorKey :: !String,
    _xmlAttributeDecodingErrorMap :: !(Map Name Text)
  }
  deriving (Eq, Show)

instance Exception XmlAttributeDecodingError

class PlexRequest request response | request -> response where
  executeRequest :: PlexIp -> PlexToken -> request -> IO response

class FromXmlDocument a where
  fromXmlDocument :: (MonadThrow m) => Document -> m a

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

data PlexDevice = PlexDevice
  { _plexDeviceId :: !Integer,
    _plexDeviceName :: !String,
    _plexDeviceClientIdentifier :: !PlexClientIdentifier
  }
  deriving (Eq, Show)

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
