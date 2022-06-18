module Plex.PlexRequest where

import Control.Lens.Combinators
import Data.Kind (Type)
import Data.Typeable (typeRep)
import qualified Data.UUID as UUID
import Plex.Http (callRoute)
import Qtility
import qualified RIO.Text as Text
import Text.XML
import Text.XML.Lens
import Types

class PlexRequest request where
  type ResponseType request :: Type
  executeRequest :: PlexIp -> PlexToken -> request -> IO (ResponseType request)

data GetDevicesRequest = GetDevicesRequest

newtype GetDevicesResponse = GetDevicesResponse {unGetDevicesResponse :: [PlexDevice]}
  deriving (Eq, Show)

data GetLibrarySectionsRequest = GetLibrarySectionsRequest

newtype GetLibrariesResponse = GetLibrariesResponse {unGetLibrariesResponse :: [PlexDirectory]}
  deriving (Eq, Show)

instance PlexRequest GetDevicesRequest where
  type ResponseType GetDevicesRequest = GetDevicesResponse
  executeRequest ip token _request = do
    document <- callRoute ip token "devices"
    let attributes = document ^.. root . named "MediaContainer" . plate . named "Device" . attrs
    devices <- fromEither $ traverse parseDevice attributes
    pure $ GetDevicesResponse devices

instance PlexRequest GetLibrarySectionsRequest where
  type ResponseType GetLibrarySectionsRequest = GetLibrariesResponse
  executeRequest ip token _request = do
    document <- callRoute ip token "library/sections"
    directories <-
      fromEither $
        traverse parseDirectory $
          document ^.. root . named "MediaContainer" . plate . named "Directory"
    pure $ GetLibrariesResponse directories

parseDevice :: Map Name Text -> Either XmlAttributeError PlexDevice
parseDevice attributeMap = do
  deviceId <- getAttribute attributeMap "id"
  name' <- (emptyToNothing >>> fmap Text.unpack) <$> getAttribute attributeMap "name"
  clientIdentifier <- PlexClientIdentifier <$> getAttribute attributeMap "clientIdentifier"
  createdAt <- PlexTimestamp <$> getAttribute attributeMap "createdAt"
  platform <- (emptyToNothing >>> fmap PlexPlatform) <$> getAttribute attributeMap "platform"
  pure $
    PlexDevice
      { _plexDeviceId = deviceId,
        _plexDeviceName = name',
        _plexDeviceClientIdentifier = clientIdentifier,
        _plexDeviceCreatedAt = createdAt,
        _plexDevicePlatform = platform
      }

parseDirectory :: Element -> Either XmlAttributeError PlexDirectory
parseDirectory element' = do
  locations <- traverse parseLocation $ element' ^.. plate . named "Location" . attrs
  let attributes = element' ^. attrs
  key' <- PlexKey <$> getAttribute attributes "key"
  type' <- getAttribute attributes "type" >>= readDirectoryType attributes "type"
  thumbnail <- getAttribute attributes "thumb"
  title <- getAttribute attributes "title"
  agent <- PlexAgent <$> getAttribute attributes "agent"
  scanner <- PlexScanner <$> getAttribute attributes "scanner"
  uuid <- getAttribute attributes "uuid" >>= textToUuid attributes "uuid"
  updatedAt <- PlexTimestamp <$> getAttribute attributes "updatedAt"
  createdAt <- PlexTimestamp <$> getAttribute attributes "createdAt"
  scannedAt <- PlexTimestamp <$> getAttribute attributes "scannedAt"
  contentChangedAt <- PlexTimestamp <$> getAttribute attributes "contentChangedAt"
  hidden <- getAttribute attributes "hidden" >>= numberTextToBool attributes "hidden"
  pure $
    PlexDirectory
      { _plexDirectoryKey = key',
        _plexDirectoryType = type',
        _plexDirectoryThumbnail = thumbnail,
        _plexDirectoryTitle = title,
        _plexDirectoryAgent = agent,
        _plexDirectoryScanner = scanner,
        _plexDirectoryUuid = uuid,
        _plexDirectoryUpdatedAt = updatedAt,
        _plexDirectoryCreatedAt = createdAt,
        _plexDirectoryScannedAt = scannedAt,
        _plexDirectoryContentChangedAt = contentChangedAt,
        _plexDirectoryHidden = hidden,
        _plexDirectoryLocations = locations
      }

parseLocation :: Map Name Text -> Either XmlAttributeError PlexLocation
parseLocation attributeMap = do
  id' <- PlexId <$> getAttribute attributeMap "id"
  path <- getAttribute attributeMap "path"
  pure $ PlexLocation {_plexLocationId = id', _plexLocationPath = path}

getAttribute ::
  forall a.
  (PlexAttributeRead a, Typeable a) =>
  Map Name Text ->
  Name ->
  Either XmlAttributeError a
getAttribute attributeMap key' = do
  value <-
    note
      ( XmlAttributeError
          attributeMap
          (DoesNotExistType $ DoesNotExist (PlexAttributeKey $ nameLocalName key'))
      )
      (attributeMap ^. at key')
  let reason = Just $ mconcat ["Unable to `read` value as ", show $ typeRep (Proxy @a)]
  note
    ( XmlAttributeError
        attributeMap
        (UnableToDecodeType $ UnableToDecode key' value reason)
    )
    $ readAttribute value

emptyToNothing :: Text -> Maybe Text
emptyToNothing "" = Nothing
emptyToNothing t = Just t

ensureNonEmpty ::
  Foldable f =>
  Map Name Text ->
  Name ->
  Text ->
  f a ->
  Either XmlAttributeError (f a)
ensureNonEmpty attributeMap key' textValue value
  | null value =
    Left $
      XmlAttributeError
        attributeMap
        (UnableToDecodeType $ UnableToDecode key' textValue $ Just "value is empty")
  | otherwise = Right value

readDirectoryType :: Map Name Text -> Name -> Text -> Either XmlAttributeError PlexDirectoryType
readDirectoryType _map' _key' "movie" = Right MovieDirectory
readDirectoryType _map' _key' "show" = Right ShowDirectory
readDirectoryType map' key' value =
  Left $
    XmlAttributeError map' $
      UnableToDecodeType $
        UnableToDecode key' value (Just $ "Unknown PlexDirectoryType: " <> Text.unpack value)

numberTextToBool :: Map Name Text -> Name -> Text -> Either XmlAttributeError Bool
numberTextToBool _map' _key' "1" = Right True
numberTextToBool _map' _key' "0" = Right False
numberTextToBool map' key' value =
  Left $
    XmlAttributeError
      map'
      $ UnableToDecodeType $
        UnableToDecode
          key'
          value
          (Just $ "Unknown numberTextToBool: " <> Text.unpack value)

textToUuid :: Map Name Text -> Name -> Text -> Either XmlAttributeError UUID
textToUuid map' key' value =
  case UUID.fromText value of
    Just uuid -> Right uuid
    Nothing ->
      Left $
        XmlAttributeError
          map'
          $ UnableToDecodeType $
            UnableToDecode
              key'
              value
              (Just $ "Unknown UUID: " <> Text.unpack value)
