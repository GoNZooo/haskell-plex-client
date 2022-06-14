module Plex.Http where

import Network.HTTP.Client (Response (..), httpLbs, parseRequest)
import Network.HTTP.Client.TLS (newTlsManager)
import Qtility
import Text.XML
import Types

callRoute :: PlexIp -> PlexToken -> String -> IO Document
callRoute ip token route = do
  manager <- newTlsManager
  let url = "http://" ++ unPlexIp ip ++ ":32400/" ++ route ++ "?X-Plex-Token=" ++ unPlexToken token
  request <- parseRequest url
  response <- httpLbs request manager

  response & responseBody & parseLBS def & mapLeft XmlDecodingError & fromEither

apiUrl :: PlexIp -> PlexToken -> String -> String
apiUrl ip token route =
  mconcat ["https://", unPlexIp ip, ":32400/", route, "?X-Plex-Token=", unPlexToken token]
