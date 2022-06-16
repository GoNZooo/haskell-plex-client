{-# LANGUAGE TemplateHaskell #-}

module Types where

import Qtility
import RIO.Process (HasProcessContext (..), ProcessContext)
import qualified RIO.Text as Text
import Text.XML (Name)

newtype XmlDecodingError = XmlDecodingError {unXmlDecodingError :: SomeException}
  deriving (Show)

instance Exception XmlDecodingError

newtype PlexAttributeKey = PlexAttributeKey {unPlexAttributeKey :: Text}
  deriving (Show, Eq, Ord)

newtype DoesNotExist = DoesNotExist {unDoesNotExist :: PlexAttributeKey}
  deriving (Show, Eq, Ord)

data XmlAttributeError = XmlAttributeError
  { _xmlAttributeErrorMap :: !(Map Name Text),
    _xmlAttributeErrorType :: !XmlAttributeErrorType
  }
  deriving (Eq, Show)

instance Exception XmlAttributeError

data XmlAttributeErrorType
  = DoesNotExistType !DoesNotExist
  | UnableToDecodeType !UnableToDecode
  deriving (Eq, Show)

data UnableToDecode = UnableToDecode
  { _unableToDecodeName :: !Name,
    _unableToDecodeValue :: !Text,
    _unableToDecodeReason :: !(Maybe String)
  }
  deriving (Eq, Show)

class PlexAttributeRead a where
  readAttribute :: Text -> Maybe a

instance PlexAttributeRead Text where
  readAttribute = Just

instance PlexAttributeRead String where
  readAttribute = Text.unpack >>> Just

instance PlexAttributeRead ByteString where
  readAttribute = encodeUtf8 >>> Just

instance PlexAttributeRead Int where
  readAttribute = Text.unpack >>> readMaybe

instance PlexAttributeRead Integer where
  readAttribute = Text.unpack >>> readMaybe

instance PlexAttributeRead Bool where
  readAttribute = Text.unpack >>> readMaybe

newtype PlexToken = PlexToken {unPlexToken :: String}
  deriving (Eq, Show, Read, IsString, Generic, FromEnvironmentValue)

class HasPlexToken env where
  plexTokenL :: Lens' env PlexToken

instance HasPlexToken PlexToken where
  plexTokenL = id

newtype PlexIp = PlexIp {unPlexIp :: String}
  deriving (Eq, Show, Read, IsString, Generic, FromEnvironmentValue)

newtype PlexId = PlexId {unPlexId :: Integer}
  deriving (Eq, Show, Read, Generic, FromEnvironmentValue)

newtype PlexAgent = PlexAgent {unPlexAgent :: String}
  deriving (Eq, Show, Read, Generic, IsString, FromEnvironmentValue)

newtype PlexScanner = PlexScanner {unPlexScanner :: String}
  deriving (Eq, Show, Read, Generic, IsString, FromEnvironmentValue)

class HasPlexIp env where
  plexIpL :: Lens' env PlexIp

instance HasPlexIp PlexIp where
  plexIpL = id

newtype PlexClientIdentifier = PlexClientIdentifier {unPlexClientIdentifier :: Text}
  deriving (Eq, Show, Read, IsString, Generic)

newtype PlexTimestamp = PlexTimestamp {unPlexTimestamp :: Integer}
  deriving (Eq, Show, Read, Generic)

newtype PlexPlatform = PlexPlatform {unPlexPlatform :: Text}
  deriving (Eq, Show, Read, IsString, Generic)

newtype PlexKey = PlexKey {unPlexKey :: String}
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
    _plexDeviceName :: !(Maybe String),
    _plexDeviceClientIdentifier :: !PlexClientIdentifier,
    _plexDeviceCreatedAt :: !PlexTimestamp,
    _plexDevicePlatform :: !(Maybe PlexPlatform)
  }
  deriving (Eq, Show)

data PlexLocation = PlexLocation
  { _plexLocationId :: !PlexId,
    _plexLocationPath :: !FilePath
  }
  deriving (Eq, Show)

data PlexDirectory = PlexDirectory
  { _plexDirectoryKey :: !PlexKey,
    _plexDirectoryType :: !PlexDirectoryType,
    _plexDirectoryThumbnail :: !FilePath,
    _plexDirectoryTitle :: !Text,
    _plexDirectoryAgent :: !PlexAgent,
    _plexDirectoryScanner :: !PlexScanner,
    _plexDirectoryUuid :: !UUID,
    _plexDirectoryUpdatedAt :: !PlexTimestamp,
    _plexDirectoryCreatedAt :: !PlexTimestamp,
    _plexDirectoryScannedAt :: !PlexTimestamp,
    _plexDirectoryContentChangedAt :: !PlexTimestamp,
    _plexDirectoryHidden :: !Bool,
    _plexDirectoryLocations :: ![PlexLocation]
  }
  deriving (Eq, Show)

data PlexDirectoryType
  = MovieDirectory
  | ShowDirectory
  deriving (Eq, Show, Ord)

foldMapM makeLenses [''Options, ''App, ''PlexDevice, ''PlexLocation, ''PlexDirectory]

foldMapM
  makeWrapped
  [ ''PlexToken,
    ''PlexIp,
    ''XmlDecodingError,
    ''PlexTimestamp,
    ''PlexPlatform,
    ''PlexClientIdentifier,
    ''DoesNotExist,
    ''PlexAttributeKey,
    ''PlexId
  ]

instance HasLogFunc App where
  logFuncL = appLogFunc

instance HasProcessContext App where
  processContextL = appProcessContext

instance HasPlexToken App where
  plexTokenL = appPlexToken

instance HasPlexIp App where
  plexIpL = appPlexIp
