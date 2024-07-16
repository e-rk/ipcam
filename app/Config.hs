module Config
  ( configEncode,
    configDecode,
  )
where

import Data.Bifunctor
import Data.ByteString
import Data.CaseInsensitive
import Data.Text
import Toml (TomlCodec, (.=))
import Toml qualified
import Types
  ( AppConfig (..),
    Camera (..),
    Config (..),
    PtzCommand (..),
    PtzData (..),
    PtzRequest (..),
    URI,
    textToUri,
    uriToText,
  )

tomlUri :: Toml.Key -> TomlCodec URI
tomlUri = Toml.textBy uriToText textToUri

tomlPtzCommand :: TomlCodec PtzCommand
tomlPtzCommand = Toml.enumBounded "command"

tomlCIbyteString :: Toml.Key -> TomlCodec (CI ByteString)
tomlCIbyteString key = mk <$> (Toml.byteString key) .= original

tomlPtzRequest :: TomlCodec PtzRequest
tomlPtzRequest =
  PtzRequest
    <$> tomlUri "requestURI" .= requestURI
    <*> Toml.text "requestStartBody" .= requestStartBody
    <*> Toml.text "requestStopBody" .= requestStopBody

ptzActionsCodec :: TomlCodec PtzData
ptzActionsCodec =
  PtzData
    <$> Toml.list (Toml.pair (tomlCIbyteString "key") (Toml.byteString "value")) "requestHeader" .= requestHeaders
    <*> Toml.text "requestMethod" .= requestMethod
    <*> Toml.map (tomlPtzCommand) tomlPtzRequest "ptzRequestData" .= requestCommands

cameraCodec :: TomlCodec Camera
cameraCodec =
  Camera
    <$> Toml.text "name" .= name
    <*> tomlUri "videoURI" .= videoURI
    <*> Toml.table (Toml.dioptional ptzActionsCodec) "ptzActions" .= ptzActions

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> Toml.map (Toml.text "name") cameraCodec "cameras" .= cameras
    <*> Toml.table appConfigCodec "app" .= appConfig

appConfigCodec :: TomlCodec AppConfig
appConfigCodec = AppConfig <$> Toml.bool "autostart" .= autostart

configEncode :: Config -> Text
configEncode = Toml.encode configCodec

configDecode :: Text -> Either Text Config
configDecode config = bimap Toml.prettyTomlDecodeErrors id (decode config)
  where
    decode = Toml.decode configCodec
