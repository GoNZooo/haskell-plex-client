module Plex.PlexRequest where

import Control.Lens.Combinators
import Data.Typeable (typeRep)
import Plex.Http (callRoute)
import Qtility
import qualified RIO.Text as Text
import Text.XML
import Text.XML.Lens
import Types

class PlexRequest request response | request -> response where
  executeRequest :: PlexIp -> PlexToken -> request -> IO response

data GetDevicesRequest = GetDevicesRequest

newtype GetDevicesResponse = GetDevicesResponse {unGetDevicesResponse :: [PlexDevice]}
  deriving (Eq, Show)

data GetLibrariesRequest = GetLibrariesRequest

newtype GetLibrariesResponse = GetLibrariesResponse {unGetLibrariesResponse :: [PlexDirectory]}
  deriving (Eq, Show)

instance PlexRequest GetDevicesRequest GetDevicesResponse where
  executeRequest ip token _request = do
    document <- callRoute ip token "devices"
    let attributes = document ^.. root . named "MediaContainer" . plate . named "Device" . attrs
    devices <- fromEither $ traverse parseDevice attributes
    pure $ GetDevicesResponse devices

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
