module Main where

import Config
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.ByteString.Lazy qualified as LBS
import Data.GI.Base (AttrOp ((:=)))
import Data.GI.Base qualified as Gptr
import Data.GI.Base.Properties qualified as Gptr
import Data.Map qualified as Map
import Data.Maybe
import Data.Text hiding (find, foldl', foldr, null)
import Data.Text.Encoding
import Data.Text.IO.Utf8 qualified as T
import Data.Tuple
import GI.Gst qualified as Gst
import Gui
import Network.HTTP.Simple
import Storage qualified as St
import System.Environment
import System.Environment.XDG.BaseDir qualified as Xdg
import Types

data Runtime = Runtime
  { gui :: Gui,
    playbin :: Gst.Pipeline,
    config :: AppConfig,
    storage :: St.Storage
  }

type MRuntime = MVar Runtime

type App = StateT Runtime IO

cameraURI :: Camera -> Text
cameraURI = uriToText . videoURI

gstCreateGtkWidget :: Gui -> Gst.Pipeline -> IO ()
gstCreateGtkWidget gui pipeline = do
  gtkglsink <- withGui gui guiGstCreateVideoSink
  Just glsinkbin <- Gst.elementFactoryMake "glsinkbin" (Just "glsinkbin")
  Gptr.setObjectPropertyObject pipeline "video-sink" (Just glsinkbin)
  Gptr.setObjectPropertyObject glsinkbin "sink" gtkglsink

getConfigPath :: IO String
getConfigPath = Xdg.getUserConfigFile "ipcam" "camera.toml"

loadConfig :: IO (Either Text Config)
loadConfig = do
  cameraFilePath <- getConfigPath
  config <- T.readFile cameraFilePath
  pure $ configDecode config

storeConfig :: Config -> IO ()
storeConfig config = do
  cameraFilePath <- getConfigPath
  T.writeFile cameraFilePath (configEncode config)

getRuntime :: App Runtime
getRuntime = get

getConfig :: App AppConfig
getConfig = config <$> getRuntime

addCameraSelectionI :: Camera -> App ()
addCameraSelectionI camera = do
  runtime <- getRuntime
  liftIO $ withGui runtime.gui $ guiAddCamera camera

updateCameraSelection :: App ()
updateCameraSelection = do
  runtime <- getRuntime
  liftIO $ withGui runtime.gui guiUpdateCameraList

saveConfig :: App ()
saveConfig = do
  runtime <- getRuntime
  cameras <- getCameras
  let config = Config {appConfig = runtime.config, cameras = cameras}
  liftIO $ storeConfig config

addCameraI :: Camera -> App Bool
addCameraI camera = do
  runtime <- getRuntime
  existingCamera <- St.getCamera runtime.storage camera.name
  case (isNothing existingCamera) of
    True -> do
      liftIO $ St.addCamera runtime.storage camera
      saveConfig
      addCameraSelectionI camera
      pure True
    False -> pure False

selectCamera :: Text -> App ()
selectCamera name = do
  runtime <- getRuntime
  camera <- St.getCamera runtime.storage name
  case camera of
    Just cam -> do
      _ <- Gst.elementSetState runtime.playbin Gst.StateReady
      liftIO $ Gptr.setObjectPropertyString runtime.playbin "uri" (Just $ cameraURI cam)
      Gst.set runtime.playbin [#latency := 300]
      _ <- Gst.elementSetState runtime.playbin Gst.StatePlaying
      pure ()
    Nothing -> pure ()

getCameras :: App CameraMap
getCameras = do
  runtime <- getRuntime
  St.getCameras runtime.storage

initApp :: App ()
initApp = do
  runtime <- getRuntime
  liftIO $ withGui runtime.gui guiUpdateCameraList

runApp :: MRuntime -> App a -> IO a
runApp runtime = runner
  where
    runner action = modifyMVar runtime (\x -> swap <$> runStateT action x)

selectCameraHandler :: MRuntime -> Text -> IO ()
selectCameraHandler runtime name = do
  _ <- runApp runtime (selectCamera name)
  pure ()

appRemoveCurrentCamera :: App ()
appRemoveCurrentCamera = do
  runtime <- getRuntime
  cameraName <- liftIO $ withGui runtime.gui guiGetSelectedCameraName
  case cameraName of
    Just name -> do
      St.removeCamera runtime.storage name
      updateCameraSelection
      saveConfig
    Nothing -> pure ()

appEditCurrentCamera :: App ()
appEditCurrentCamera = do
  runtime <- getRuntime
  selectedName <- liftIO $ withGui runtime.gui guiGetSelectedCameraName
  camera <- case selectedName of
    Just name -> St.getCamera runtime.storage name
    Nothing -> pure Nothing
  case camera of
    Just cam -> liftIO $ withGui runtime.gui (guiOpenEditCameraDialog cam)
    Nothing -> pure ()

getCurrentCamera :: App (Maybe Camera)
getCurrentCamera = do
  runtime <- getRuntime
  cameraName <- liftIO $ withGui runtime.gui guiGetSelectedCameraName
  case cameraName of
    Just name -> St.getCamera runtime.storage name
    Nothing -> pure Nothing

appEditedCurrentCamera :: Camera -> App Bool
appEditedCurrentCamera camera = do
  runtime <- getRuntime
  selectedName <- liftIO $ withGui runtime.gui guiGetSelectedCameraName
  case selectedName of
    Just name -> do
      result <- St.overwriteCamera runtime.storage name camera
      updateCameraSelection
      saveConfig
      pure result
    Nothing -> pure False

appPtzAction :: PtzCommand -> Bool -> Maybe PtzData -> App ()
appPtzAction ptzCommand start (Just (PtzData requestHeaders requestMethod ptzData)) = do
  let request = Map.lookup ptzCommand ptzData
  case request of
    Just requestData -> do
      let requestBody = if start then requestData.requestStartBody else requestData.requestStopBody
      reqInit <- parseRequest (unpack $ uriToText requestData.requestURI)
      let req =
            setRequestMethod (encodeUtf8 requestMethod) $
              setRequestBodyLBS (LBS.fromStrict $ encodeUtf8 requestBody) reqInit
      let req2 = foldr (\a b -> addRequestHeader (fst a) (snd a) b) req requestHeaders
      _ <- httpLBS req2
      pure ()
    Nothing -> pure ()
  pure ()
appPtzAction _ _ (Nothing) = pure ()

appPtzUp :: PtzCommand -> Bool -> App ()
appPtzUp ptzCommand buttonState = do
  camera <- getCurrentCamera
  case camera of
    Just cam -> do
      appPtzAction ptzCommand buttonState cam.ptzActions
    Nothing -> pure ()

setupHandlers :: MRuntime -> Gui -> IO ()
setupHandlers runtime gui = do
  let handlers =
        GuiHandlers
          { addCameraHandler = (\camera -> runApp runtime (addCameraI camera)),
            editCameraHandler = (\camera -> runApp runtime (appEditedCurrentCamera camera)),
            ptzClicked = (\ptzCommand buttonState -> runApp runtime (appPtzUp ptzCommand buttonState)),
            exitHandler = undefined,
            activateHandler = runApp runtime initApp,
            cameraSelectedHandler = (\name -> runApp runtime (selectCamera name)),
            editCameraActionHandler = runApp runtime appEditCurrentCamera,
            deleteCameraActionHandler = runApp runtime appRemoveCurrentCamera
          }
  withGui gui $ guiSetHandlers handlers

main :: IO ()
main = do
  Right cfg <- loadConfig
  storage <- St.newStorage cfg.cameras
  (gui, app) <- guiInit storage
  args <- getArgs
  _ <- Gst.init (Just $ fmap pack args)
  pipeline <- Gst.elementFactoryMake "playbin3" (Just "pipeline") >>= Gptr.unsafeCastTo Gst.Pipeline . fromJust
  gstCreateGtkWidget gui pipeline

  newMVar Runtime {gui = gui, playbin = pipeline, config = cfg.appConfig, storage = storage} >>= (\x -> setupHandlers x gui)
  -- case cfg of
  --   Right cfg' -> newMVar Runtime {gui = gui, playbin = pipeline, config = cfg'} >>= (\x -> setupHandlers x gui)
  --   Left _ -> error ""
  guiProcessEvents app
