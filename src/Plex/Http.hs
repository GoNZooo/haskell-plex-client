module Plex.Http where

import Control.Lens.Combinators (plate)
import Network.HTTP.Client (Response (..), httpLbs, parseRequest)
import Network.HTTP.Client.TLS (newTlsManager)
import Qtility
import qualified RIO.Text as Text
import Text.XML
import Text.XML.Lens
import Types

data GetDevicesRequest = GetDevicesRequest

newtype GetDevicesResponse = GetDevicesResponse {unGetDevicesResponse :: [PlexDevice]}
  deriving (Eq, Show)

instance PlexRequest GetDevicesRequest GetDevicesResponse where
  executeRequest ip token _request = do
    document <- callRoute ip token "devices"
    let attributes = document ^.. root . named "MediaContainer" . plate . named "Device" . attrs
    devices <- fromEither $ sequence $ createDevice <$> attributes
    pure $ GetDevicesResponse devices -- _x <$> (document & documentRoot & elementNodes)
    where
      createDevice :: Map Name Text -> Either XmlAttributeDecodingError PlexDevice
      createDevice attributeMap = do
        deviceId <- do
          deviceIdText <-
            Text.unpack
              <$> note (XmlAttributeDecodingError "id" attributeMap) (attributeMap ^. at "id")
          note (XmlAttributeDecodingError "id" attributeMap) $ readMaybe deviceIdText
        name' <-
          Text.unpack
            <$> note (XmlAttributeDecodingError "name" attributeMap) (attributeMap ^. at "name")
        clientIdentifier <-
          PlexClientIdentifier
            <$> note
              (XmlAttributeDecodingError "clientIdentifier" attributeMap)
              (attributeMap ^. at "clientIdentifier")
        createdAt <- do
          createdAtText <-
            Text.unpack
              <$> note
                (XmlAttributeDecodingError "createdAt" attributeMap)
                (attributeMap ^. at "createdAt")
          PlexTimestamp <$> note (XmlAttributeDecodingError "createdAt" attributeMap) (readMaybe createdAtText)
        platform <-
          PlexPlatform
            <$> note (XmlAttributeDecodingError "platform" attributeMap) (attributeMap ^. at "platform")
        pure $
          PlexDevice
            { _plexDeviceId = deviceId,
              _plexDeviceName = name',
              _plexDeviceClientIdentifier = clientIdentifier,
              _plexDeviceCreatedAt = createdAt,
              _plexDevicePlatform = platform
            }

callRoute :: (MonadIO m, MonadThrow m) => PlexIp -> PlexToken -> String -> m Document
callRoute ip token route = do
  manager <- liftIO newTlsManager
  let url = "http://" ++ unPlexIp ip ++ ":32400/" ++ route ++ "?X-Plex-Token=" ++ unPlexToken token
  request <- parseRequest url
  response <- liftIO $ httpLbs request manager

  response & responseBody & parseLBS def & mapLeft XmlDecodingError & fromEither

apiUrl :: PlexIp -> PlexToken -> String -> String
apiUrl ip token route =
  mconcat ["https://", unPlexIp ip, ":32400/", route, "?X-Plex-Token=", unPlexToken token]
