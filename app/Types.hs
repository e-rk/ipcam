module Types
  ( Camera (..),
    Config (..),
    AppConfig (..),
    PtzData (..),
    PtzCommand (..),
    PtzRequest (..),
    PtzRequestCommands,
    CameraMap,
    URI,
    uriToText,
    textToUri,
    initCamera,
    setPtzData,
    rightToMaybe,
  )
where

import Data.Map qualified as Map
import Data.Text
import Network.HTTP.Simple
import Network.URI (URI, parseAbsoluteURI, uriToString)

type CameraMap = Map.Map Text Camera

data PtzCommand
  = PtzUp
  | PtzRight
  | PtzDown
  | PtzLeft
  | PtzZoomIn
  | PtzZoomOut
  deriving (Show, Eq, Enum, Ord, Bounded)

data PtzRequest = PtzRequest
  { requestURI :: URI,
    requestStartBody :: Text,
    requestStopBody :: Text
  }
  deriving (Show, Eq)

type PtzRequestCommands = Map.Map PtzCommand PtzRequest

data PtzData = PtzData
  { requestHeaders :: RequestHeaders,
    requestMethod :: Text,
    requestCommands :: PtzRequestCommands
  }
  deriving (Show, Eq)

data Camera = Camera
  { name :: Text,
    videoURI :: URI,
    ptzActions :: Maybe PtzData
  }
  deriving (Show, Eq)

data AppConfig = AppConfig {autostart :: Bool} deriving (Show)

data Config = Config
  { cameras :: !CameraMap,
    appConfig :: !AppConfig
  }
  deriving (Show)

uriToText :: URI -> Text
uriToText uri = pack $ (uriToString id uri) ""

textToUri :: Text -> Either Text URI
textToUri text = case uri of
  Just uri' -> Right uri'
  Nothing -> Left "Unable to parse URI"
  where
    uri = parseAbsoluteURI (unpack text)

initCamera :: Text -> URI -> Camera
initCamera name uri = Camera name uri Nothing

setPtzData :: PtzData -> Camera -> Camera
setPtzData actions camera = camera {ptzActions = Just actions}

rightToMaybe :: Either b a -> Maybe a
rightToMaybe (Right a) = Just a
rightToMaybe (Left _) = Nothing
