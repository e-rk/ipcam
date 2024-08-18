{-# LANGUAGE BlockArguments #-}

module Gui
  ( Gui,
    GuiHandlers (..),
    guiInit,
    withGui,
    guiAddCamera,
    guiGstCreateVideoSink,
    guiMessageDialog,
    guiProcessEvents,
    guiGetSelectedCameraName,
    guiSetHandlers,
    guiUpdateCameraList,
    guiOpenEditCameraDialog,
    guiShowBanner,
  )
where

import CameraActionRow
import CameraOptions
import CameraPtzConfig
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.GI.Base (AttrOp ((:=)))
import Data.GI.Base qualified as Gptr
import Data.GI.Base.GObject qualified as Gobj
import Data.GI.Base.Properties qualified as Gptr
import Data.GI.Base.ShortPrelude (gvariantFromText)
import Data.IORef
import Data.Maybe
import Data.Text qualified as T
import Data.Word
import GI.Adw qualified as Adw
import GI.GObject qualified as Gobj
import GI.Gdk qualified as Gdk
import GI.Gio qualified as Gio
import GI.Gst qualified as Gst
import GI.Gtk qualified as Gtk
import Paths_ipcam (getDataFileName)
import Storage qualified as St (Storage, getCamera, getCamerasList)
import System.Environment
import Types

data GuiHandlers = GuiHandlers
  { addCameraHandler :: Camera -> IO Bool,
    editCameraHandler :: Camera -> IO Bool,
    ptzClicked :: PtzCommand -> Bool -> IO (),
    exitHandler :: IO (),
    activateHandler :: IO (),
    cameraSelectedHandler :: T.Text -> IO (),
    editCameraActionHandler :: IO (),
    deleteCameraActionHandler :: IO (),
    connectRequestHandler :: IO ()
  }

data GuiInternal = GuiInternal
  { application :: Gtk.Application,
    applicationWindow :: Adw.ApplicationWindow,
    cameraAddButton :: Gtk.Button,
    cameraMenuPopover :: Gtk.PopoverMenu,
    cameraListStore :: Gtk.StringList,
    cameraListSelection :: Gtk.SingleSelection,
    ptzUpButton :: Gtk.Button,
    ptzRightButton :: Gtk.Button,
    ptzDownButton :: Gtk.Button,
    ptzLeftButton :: Gtk.Button,
    ptzZoomInButton :: Gtk.Button,
    ptzZoomOutButton :: Gtk.Button,
    cameraPicture :: Gtk.Picture,
    errorDialog :: Adw.AlertDialog,
    errorDetails :: Gtk.TextView,
    editCameraAction :: Gio.SimpleAction,
    deleteCameraAction :: Gio.SimpleAction,
    guiHandlers :: IORef (Maybe GuiHandlers),
    cameraHandler :: IORef (GuiHandlers -> (Camera -> IO Bool)),
    cameraStorage :: St.Storage,
    cameraOptions :: CameraOptions,
    banner :: Adw.Banner,
    cameraNavigation :: Adw.NavigationSplitView
  }

type Gui = GuiInternal

type GuiContext a = ReaderT GuiInternal IO a

guiInit :: St.Storage -> IO (Gui, Gtk.Application)
guiInit storage = do
  adwapp <- Adw.applicationNew (Just "io.github.e_rk.ipcam") [Gio.ApplicationFlagsDefaultFlags]

  -- Register all custom types
  _ <- Gobj.registerGType CameraActionRow
  _ <- Gobj.registerGType CameraPtzConfig

  app <- Gtk.toApplication adwapp
  resourcePath <- getDataFileName "resources/resources.gresource"
  gresource <- Gio.resourceLoad resourcePath
  Gio.resourcesRegister gresource
  Gio.applicationRegister app (Nothing :: Maybe Gio.Cancellable)
  Gtk.setApplicationRegisterSession app True
  builder <- Gtk.builderNewFromResource "/io/github/e_rk/ipcam/gui.xml"
  applicationWindow <- initWindow app builder
  cameraAddButton <- Gtk.builderGetObject builder "cameraAddButton" >>= Gptr.unsafeCastTo Gtk.Button . fromJust
  cameraMenuPopover <- Gtk.builderGetObject builder "cameraMenuPopover" >>= Gptr.unsafeCastTo Gtk.PopoverMenu . fromJust
  cameraListStore <- Gtk.builderGetObject builder "store" >>= Gptr.unsafeCastTo Gtk.StringList . fromJust
  cameraListSelection <- Gtk.builderGetObject builder "cameraSelection" >>= Gptr.unsafeCastTo Gtk.SingleSelection . fromJust
  ptzUp <- Gtk.builderGetObject builder "ptzUp" >>= Gptr.unsafeCastTo Gtk.Button . fromJust
  ptzRight <- Gtk.builderGetObject builder "ptzRight" >>= Gptr.unsafeCastTo Gtk.Button . fromJust
  ptzDown <- Gtk.builderGetObject builder "ptzDown" >>= Gptr.unsafeCastTo Gtk.Button . fromJust
  ptzLeft <- Gtk.builderGetObject builder "ptzLeft" >>= Gptr.unsafeCastTo Gtk.Button . fromJust
  ptzZoomIn <- Gtk.builderGetObject builder "ptzZoomIn" >>= Gptr.unsafeCastTo Gtk.Button . fromJust
  ptzZoomOut <- Gtk.builderGetObject builder "ptzZoomOut" >>= Gptr.unsafeCastTo Gtk.Button . fromJust
  picture <- Gtk.builderGetObject builder "videoPicture" >>= Gptr.unsafeCastTo Gtk.Picture . fromJust
  banner <- Gtk.builderGetObject builder "offlineStatus" >>= Gptr.unsafeCastTo Adw.Banner . fromJust
  cameraNavigation <- Gtk.builderGetObject builder "split_view" >>= Gptr.unsafeCastTo Adw.NavigationSplitView . fromJust

  errorDialogBuilder <- Gtk.builderNewFromResource "/io/github/e_rk/ipcam/error-dialog.ui"
  errorDialog <- Gtk.builderGetObject errorDialogBuilder "errorDialog" >>= Gptr.unsafeCastTo Adw.AlertDialog . fromJust
  errorDetails <- Gtk.builderGetObject errorDialogBuilder "errorDetails" >>= Gptr.unsafeCastTo Gtk.TextView . fromJust

  menuBuilder <- Gtk.builderNewFromResource "/io/github/e_rk/ipcam/item-menu.ui"
  model <- Gtk.builderGetObject menuBuilder "app-menu" >>= Gptr.unsafeCastTo Gio.MenuModel . fromJust
  _ <- Gtk.popoverMenuSetMenuModel cameraMenuPopover (Just model)

  editCameraAction <- Gio.new Gio.SimpleAction [#name := "edit-camera"]
  deleteCameraAction <- Gio.new Gio.SimpleAction [#name := "delete-camera"]

  Gio.actionMapAddAction app editCameraAction
  Gio.actionMapAddAction app deleteCameraAction

  guiHandlers <- newIORef Nothing
  cameraHandler <- newIORef addCameraHandler

  cssProvider <- Gtk.new Gtk.CssProvider []
  display <- fromJust <$> Gdk.displayGetDefault
  Gtk.cssProviderLoadFromResource cssProvider "/io/github/e_rk/ipcam/app.css"
  Gtk.styleContextAddProviderForDisplay display cssProvider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)

  cameraOptions <- Gtk.new CameraOptions []
  gui <-
    pure $
      GuiInternal
        { application = app,
          applicationWindow = applicationWindow,
          cameraAddButton = cameraAddButton,
          cameraMenuPopover = cameraMenuPopover,
          cameraListStore = cameraListStore,
          cameraListSelection = cameraListSelection,
          ptzUpButton = ptzUp,
          ptzRightButton = ptzRight,
          ptzDownButton = ptzDown,
          ptzLeftButton = ptzLeft,
          ptzZoomInButton = ptzZoomIn,
          ptzZoomOutButton = ptzZoomOut,
          cameraPicture = picture,
          errorDialog = errorDialog,
          errorDetails = errorDetails,
          editCameraAction = editCameraAction,
          deleteCameraAction = deleteCameraAction,
          guiHandlers = guiHandlers,
          cameraHandler = cameraHandler,
          cameraStorage = storage,
          cameraOptions = cameraOptions,
          banner = banner,
          cameraNavigation = cameraNavigation
        }
  registerActions gui
  pure (gui, app)

guiSetHandlers :: GuiHandlers -> GuiContext ()
guiSetHandlers guiHandlers = do
  gui <- ask
  liftIO $ writeIORef gui.guiHandlers (Just guiHandlers)

initWindow :: Gtk.Application -> Gtk.Builder -> IO Adw.ApplicationWindow
initWindow app builder = do
  mainWin <- Gtk.builderGetObject builder "mainWin" >>= Gptr.unsafeCastTo Adw.ApplicationWindow . fromJust
  Gtk.applicationAddWindow app mainWin
  Gtk.windowPresent mainWin
  pure mainWin

withGui :: Gui -> GuiContext a -> IO a
withGui = flip runReaderT

guiAddCamera :: Camera -> GuiContext ()
guiAddCamera camera = do
  store <- cameraListStore <$> ask
  #append store camera.name
  pure ()

guiUpdateCameraList :: GuiContext ()
guiUpdateCameraList = do
  gui <- ask
  cameras <- St.getCamerasList gui.cameraStorage
  num <- Gio.listModelGetNItems gui.cameraListStore
  #splice gui.cameraListStore 0 num (Just $ fmap name cameras)

cameraSelectionWrapper :: Word32 -> Word32 -> GuiContext ()
cameraSelectionWrapper _ _ = do
  gui <- ask
  selection <- guiGetSelectedCameraName
  case selection of
    Just s -> do
      withHandlers (\h -> h.cameraSelectedHandler s)
      actionValue <- liftIO $ gvariantFromText "liveView"
      result <- #activateAction gui.cameraNavigation "navigation.push" (Just actionValue)
      pure ()
    Nothing -> pure ()

guiGstCreateVideoSink :: GuiContext (Maybe Gst.Element)
guiGstCreateVideoSink = do
  gtkglsink <- Gst.elementFactoryMake "gtk4paintablesink" (Just "gtk4paintablesink")
  case gtkglsink of
    Just sink -> (liftIO $ Gptr.getObjectPropertyObject sink "paintable" Gdk.Paintable) >>= (setVideo . fromJust)
    Nothing -> liftIO $ putStrLn "gtk4paintablesink not created"
  pure gtkglsink

setVideo :: Gdk.Paintable -> GuiContext ()
setVideo paintable = do
  picture <- fmap cameraPicture ask
  Gtk.pictureSetPaintable picture (Just paintable)

guiMessageDialog :: T.Text -> GuiContext ()
guiMessageDialog text = do
  window <- applicationWindow <$> ask
  dialog <- errorDialog <$> ask
  details <- errorDetails <$> ask
  textBuffer <- Gtk.get details #buffer
  Gtk.set textBuffer [#text := text]
  Adw.dialogPresent dialog (Just window)
  pure ()

guiGetSelectedCameraName :: GuiContext (Maybe T.Text)
guiGetSelectedCameraName = do
  gui <- ask
  item <- #getSelectedItem gui.cameraListSelection
  string <- mapM (liftIO . Gptr.unsafeCastTo Gtk.StringObject) item
  case string of
    Just string' -> Just <$> Gtk.get string' #string
    Nothing -> pure Nothing

guiProcessEvents :: Gtk.Application -> IO ()
guiProcessEvents application = do
  args <- getArgs
  _ <- Gio.applicationRun application (Just args)
  pure ()

openAddCameraDialog :: GuiContext ()
openAddCameraDialog = do
  gui <- ask
  setCamera gui.cameraOptions Nothing
  Adw.dialogPresent gui.cameraOptions (Just gui.applicationWindow)

guiOpenEditCameraDialog :: Camera -> GuiContext ()
guiOpenEditCameraDialog camera = do
  gui <- ask
  setCamera gui.cameraOptions (Just camera)
  Adw.dialogPresent gui.cameraOptions (Just gui.applicationWindow)

registerActions :: Gui -> IO ()
registerActions gui = do
  _ <- Gtk.on gui.cameraAddButton #clicked (withGui gui openAddCameraDialog)
  _ <- Gtk.on gui.cameraListSelection #selectionChanged (\w1 w2 -> withGui gui (cameraSelectionWrapper w1 w2))
  _ <- Gtk.on gui.application #activate (withGui gui (withHandlers \h -> h.activateHandler))
  _ <- Gio.on gui.editCameraAction #activate (\_ -> withGui gui (withHandlers \h -> h.editCameraActionHandler))
  _ <- Gio.on gui.deleteCameraAction #activate (\_ -> withGui gui (withHandlers \h -> h.deleteCameraActionHandler))
  _ <- Gio.on gui.banner #buttonClicked (withGui gui (withHandlers \h -> h.connectRequestHandler))

  let cameraOptionHandlers =
        CameraOptionsHandlers
          { saveHandler = \cameraOptions editMode -> withGui gui (cameraEditHandler cameraOptions editMode),
            validateHandler = \name editMode -> withGui gui (cameraNameInputValidate name editMode)
          }
  setHandlers gui.cameraOptions cameraOptionHandlers

  mapM_
    (uncurry (setupPtzActions gui))
    [ (gui.ptzUpButton, PtzUp),
      (gui.ptzRightButton, PtzRight),
      (gui.ptzDownButton, PtzDown),
      (gui.ptzLeftButton, PtzLeft),
      (gui.ptzZoomInButton, PtzZoomIn),
      (gui.ptzZoomOutButton, PtzZoomOut)
    ]

setupPtzActions :: Gui -> Gtk.Button -> PtzCommand -> IO ()
setupPtzActions gui button command = do
  ptzUpButtonGesture <- Gtk.new Gtk.GestureClick [#touchOnly := False, #button := fromIntegral Gdk.BUTTON_PRIMARY]
  _ <- Gtk.on ptzUpButtonGesture #pressed (\_ _ _ -> withGui gui (ptzButtonWrapper command True))
  _ <- Gtk.on ptzUpButtonGesture #released (\_ _ _ -> withGui gui (ptzButtonWrapper command False))
  Gtk.eventControllerSetPropagationPhase ptzUpButtonGesture Gtk.PropagationPhaseCapture
  Gtk.widgetAddController button ptzUpButtonGesture
  pure ()

ptzButtonWrapper :: PtzCommand -> Bool -> GuiContext ()
ptzButtonWrapper command state = withHandlers (\h -> h.ptzClicked command state)

withHandlers :: (GuiHandlers -> IO a) -> GuiContext a
withHandlers action = do
  handlers <- guiHandlers <$> ask >>= liftIO . readIORef
  case handlers of
    Just h -> liftIO $ action h
    Nothing -> error "GUI handlers not set"

cameraEditHandler :: CameraOptions -> EditMode -> GuiContext Bool
cameraEditHandler cameraOptions AddCamera = do
  camera <- getCamera cameraOptions
  case camera of
    Just cam -> do
      withHandlers \x -> liftIO $ x.addCameraHandler cam
    Nothing -> pure False
cameraEditHandler cameraOptions EditCamera = do
  camera <- getCamera cameraOptions
  case camera of
    Just cam -> do
      withHandlers \x -> liftIO $ x.editCameraHandler cam
    Nothing -> pure False

cameraNameInputValidate :: T.Text -> EditMode -> GuiContext Bool
cameraNameInputValidate name AddCamera = do
  gui <- ask
  result <- St.getCamera gui.cameraStorage name
  pure $ isNothing result && T.length name > 0
cameraNameInputValidate name EditCamera = do
  gui <- ask
  result <- St.getCamera gui.cameraStorage name
  selected <- guiGetSelectedCameraName
  pure $ (isNothing result || any (== name) selected) && T.length name > 0

guiShowBanner :: Bool -> GuiContext ()
guiShowBanner reveal = do
  gui <- ask
  #setRevealed gui.banner reveal
