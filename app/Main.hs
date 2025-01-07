{-# LANGUAGE TypeFamilies #-}

module Main where

import Config
import Control.Concurrent
import Control.Exception (catch)
import Control.Exception.Safe (tryAny)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.ByteString.Lazy qualified as LBS
import Data.GI.Base qualified as Gptr
import Data.GI.Base.Properties qualified as Gptr
import Data.GI.Base.ShortPrelude
import Data.GI.Base.Signals qualified as Sig
import Data.List
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO.Utf8 qualified as T
import Data.Tuple
import Foreign hiding (void)
import GHC.IO.Exception (IOException)
import GI.Gst qualified as Gst
import Gui
import Network.HTTP.Simple
import Storage qualified as St
import System.Environment
import System.Environment.XDG.BaseDir qualified as Xdg
import System.Locale.SetLocale
import Text.I18N.GetText
import Types

data Runtime = Runtime
  { gui :: Gui,
    playbin :: Gst.Pipeline,
    uridecodebin :: Gst.Element,
    audioconvert :: Gst.Element,
    audiosink :: Gst.Element,
    videoconvert :: Gst.Element,
    config :: AppConfig,
    storage :: St.Storage
  }

type MRuntime = MVar Runtime

type App = StateT Runtime IO

cameraURI :: Camera -> T.Text
cameraURI = uriToText . videoURI

gstCreateGtkWidget :: Gui -> Gst.Pipeline -> Gst.Element -> IO ()
gstCreateGtkWidget gui pipeline videoconvert = do
  Just gtkglsink <- withGui gui guiGstCreateVideoSink
  void $ #add pipeline gtkglsink
  void $ #link videoconvert gtkglsink

getConfigPath :: IO String
getConfigPath = Xdg.getUserConfigFile "ipcam" "camera.toml"

loadConfig :: IO (Either T.Text Config)
loadConfig = do
  catch
    ( do
        cameraFilePath <- getConfigPath
        config <- T.readFile cameraFilePath
        pure $ configDecode config
    )
    ( \e -> do
        let err = show (e :: IOException)
        pure $ Left $ T.pack err
    )

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

startCameraStreaming :: Camera -> App ()
startCameraStreaming camera = do
  runtime <- getRuntime
  liftIO $ Gptr.setObjectPropertyString runtime.uridecodebin "uri" (Just $ cameraURI camera)
  _ <- Gst.elementSetState runtime.playbin Gst.StatePlaying
  pure ()

selectCamera :: T.Text -> App ()
selectCamera name = do
  runtime <- getRuntime
  camera <- St.getCamera runtime.storage name
  case camera of
    Just cam -> startCameraStreaming cam
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

selectCameraHandler :: MRuntime -> T.Text -> IO ()
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
      reqInit <- parseRequest (T.unpack $ uriToText requestData.requestURI)
      let req =
            setRequestMethod (T.encodeUtf8 requestMethod) $
              setRequestBodyLBS (LBS.fromStrict $ T.encodeUtf8 requestBody) reqInit
      let req2 = foldr (\a b -> addRequestHeader (fst a) (snd a) b) req requestHeaders
      _ <- liftIO $ tryAny (httpNoBody req2 >> pure ())
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

streamHandler :: Gst.Message -> App ()
streamHandler message = do
  runtime <- getRuntime
  mt <- Gst.get message #type
  forM_ mt $ \messageType -> do
    case messageType of
      Gst.MessageTypeError -> do
        _ <- #setState runtime.playbin Gst.StateNull
        liftIO $ withGui runtime.gui (guiShowBanner True)
      Gst.MessageTypeEos -> do
        _ <- #setState runtime.playbin Gst.StatePlaying
        pure ()
      Gst.MessageTypeStateChanged -> do
        (_, newState, _) <- #parseStateChanged message
        case newState of
          Gst.StatePlaying -> do
            liftIO $ withGui runtime.gui (guiShowBanner False)
            Gst.debugBinToDotFile runtime.playbin [Gst.DebugGraphDetailsAll] "Pipeline"
          Gst.StateReady -> #setState runtime.playbin Gst.StatePlaying >> pure ()
          _ -> pure ()
        pure ()
      _ -> pure ()

connectRequest :: App ()
connectRequest = do
  camera <- getCurrentCamera
  case camera of
    Just cam -> startCameraStreaming cam
    Nothing -> pure ()

setupHandlers :: MRuntime -> Gui -> Gst.Pipeline -> IO ()
setupHandlers runtime gui pipeline = do
  bus <- #getBus pipeline
  #addSignalWatch bus
  _ <- Gst.on bus #message (\m -> runApp runtime (streamHandler m))
  let handlers =
        GuiHandlers
          { addCameraHandler = (\camera -> runApp runtime (addCameraI camera)),
            editCameraHandler = (\camera -> runApp runtime (appEditedCurrentCamera camera)),
            ptzClicked = (\ptzCommand buttonState -> runApp runtime (appPtzUp ptzCommand buttonState)),
            exitHandler = undefined,
            activateHandler = runApp runtime initApp,
            cameraSelectedHandler = (\name -> runApp runtime (selectCamera name)),
            editCameraActionHandler = runApp runtime appEditCurrentCamera,
            deleteCameraActionHandler = runApp runtime appRemoveCurrentCamera,
            connectRequestHandler = runApp runtime connectRequest
          }
  withGui gui $ guiSetHandlers handlers

initDefaultConfig :: IO Config
initDefaultConfig = do
  let config = Config {appConfig = AppConfig {autostart = False}, cameras = mempty}
  storeConfig config
  pure config

padAddedHandler :: Gst.Element -> Gst.Element -> Gst.Pad -> IO ()
padAddedHandler audioconvert videoconvert pad = do
  Just videopad <- #getStaticPad videoconvert "sink"
  Just audiopad <- #getStaticPad audioconvert "sink"
  Just caps <- Gst.get pad #caps
  struct <- Gst.capsGetStructure caps 0
  padType <- #getName struct
  let actions = [("video/x-raw", void $ #link pad videopad), ("audio/x-raw", void $ #link pad audiopad)]
  let p = find (flip T.isPrefixOf padType . fst) actions
  case p of
    Just p' -> snd p'
    Nothing -> pure ()

type SourceSetupCallback = Gst.Element -> IO ()

data SourceSetupSignalInfo

instance Sig.SignalInfo SourceSetupSignalInfo where
  type HaskellCallbackType SourceSetupSignalInfo = SourceSetupCallback
  connectSignal = connectGObjectNotify

type SourceSetupCallbackC = Ptr () -> Ptr Gst.Element -> Ptr () -> IO ()

foreign import ccall "wrapper"
  mkSourceSetupCallback :: SourceSetupCallbackC -> IO (FunPtr SourceSetupCallbackC)

gobjectNotifyCallbackWrapper ::
  (Gptr.GObject o) =>
  (o -> SourceSetupCallback) ->
  Ptr () ->
  Ptr Gst.Element ->
  Ptr () ->
  IO ()
gobjectNotifyCallbackWrapper cb selfPtr pspec _ = do
  pspec' <- newManagedPtr_ pspec
  withTransient (castPtr selfPtr) $ \self -> cb self (Gst.Element pspec')

connectGObjectNotify ::
  (Gptr.GObject o) =>
  o ->
  (o -> SourceSetupCallback) ->
  Sig.SignalConnectMode ->
  Maybe T.Text ->
  IO Sig.SignalHandlerId
connectGObjectNotify obj cb mode detail = do
  cb' <- mkSourceSetupCallback (gobjectNotifyCallbackWrapper cb)
  Sig.connectSignalFunPtr obj "source-setup" cb' mode detail

test :: Gst.Element -> IO ()
test source = do
  liftIO $ Gptr.setObjectPropertyInt source "latency" 300
  liftIO $ Gptr.setObjectPropertyBool source "onvif-mode" True
  liftIO $ Gptr.setObjectPropertyBool source "is-live" True
  liftIO $ Gptr.setObjectPropertyBool source "onvif-rate-control" False
  liftIO $ Gptr.setObjectPropertyInt source "backchannel" 1

proxy :: Gst.SignalProxy Gst.Element SourceSetupSignalInfo
proxy = Sig.SignalProxy

main :: IO ()
main = do
  _ <- setLocale LC_ALL (Just "")
  _ <- bindTextDomain __MESSAGE_CATALOG_DOMAIN__ (Just __MESSAGE_CATALOG_DIR__)
  _ <- textDomain (Just __MESSAGE_CATALOG_DOMAIN__)
  _ <- bindTextDomainCodeset __MESSAGE_CATALOG_DOMAIN__ (Just "UTF8")
  cfg <-
    loadConfig >>= \x -> case x of
      Right c -> pure c
      Left _ -> initDefaultConfig
  storage <- St.newStorage cfg.cameras
  (gui, app) <- guiInit storage
  args <- getArgs
  _ <- Gst.init (Just $ fmap T.pack args)
  pipeline <- Gst.new Gst.Pipeline [#name := "Pipeline"]
  uridecodebin <- Gst.elementFactoryMake "uridecodebin" (Just "Source") >>= Gptr.unsafeCastTo Gst.Element . fromJust
  videoconvert <- Gst.elementFactoryMake "videoconvert" (Just "Videoconvert") >>= Gptr.unsafeCastTo Gst.Element . fromJust
  audioconvert <- Gst.elementFactoryMake "audioconvert" (Just "Audioconvert") >>= Gptr.unsafeCastTo Gst.Element . fromJust
  audiosink <- Gst.elementFactoryMake "autoaudiosink" (Just "Audiosink") >>= Gptr.unsafeCastTo Gst.Element . fromJust
  void $ #add pipeline uridecodebin
  void $ #add pipeline videoconvert
  void $ #add pipeline audioconvert
  void $ #add pipeline audiosink
  void $ #link audioconvert audiosink
  void $ Gst.on uridecodebin proxy test
  void $ Gst.on uridecodebin #padAdded (padAddedHandler audioconvert videoconvert)

  gstCreateGtkWidget gui pipeline videoconvert

  newMVar
    Runtime
      { gui = gui,
        playbin = pipeline,
        uridecodebin = uridecodebin,
        videoconvert = videoconvert,
        audioconvert = audioconvert,
        audiosink = audiosink,
        config = cfg.appConfig,
        storage = storage
      }
    >>= (\x -> setupHandlers x gui pipeline)
  guiProcessEvents app
