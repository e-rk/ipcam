{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module CameraOptions
  ( CameraOptions (..),
    CameraOptionsHandlers (..),
    EditMode (..),
    IsCameraOptions,
    toCameraOptions,
    setCamera,
    getCamera,
    setHandlers,
  )
where

import CameraPtzConfig
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.GI.Base (AttrOp ((:=)), GObject, ManagedPtr (..), TypedObject (glibType), unsafeCastTo, withTransient)
import Data.GI.Base.GObject
  ( DerivedGObject (..),
    GObjectClass (..),
    gobjectGetPrivateData,
    gobjectSetPrivateData,
    registerGType,
  )
import Data.GI.Base.Overloading qualified as O
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text qualified as T (Text)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import Types

newtype CameraOptions = CameraOptions (ManagedPtr CameraOptions)

instance TypedObject CameraOptions where
  glibType = registerGType CameraOptions

instance GObject CameraOptions

data EditMode = AddCamera | EditCamera

data CameraOptionsPrivate = CameraOptionsPrivate
  { saveButton :: Gtk.Button,
    cancelButton :: Gtk.Button,
    nameInput :: Adw.EntryRow,
    uriInput :: Adw.EntryRow,
    ptzConfig :: CameraPtzConfig,
    editMode :: EditMode,
    handlers :: IORef (Maybe CameraOptionsHandlers)
  }

data CameraOptionsHandlers = CameraOptionsHandlers
  { saveHandler :: CameraOptions -> EditMode -> IO Bool,
    validateHandler :: T.Text -> EditMode -> IO Bool
  }

instance DerivedGObject CameraOptions where
  type GObjectParentType CameraOptions = Adw.PreferencesDialog
  type GObjectPrivateData CameraOptions = CameraOptionsPrivate
  objectTypeName = "Ipcam-CameraOptions"
  objectClassInit = cameraOptionsClassInit
  objectInstanceInit = cameraOptionsInstanceInit
  objectInterfaces = []

instance O.HasParentTypes CameraOptions

type instance O.ParentTypes CameraOptions = Adw.PreferencesDialog ': O.ParentTypes Adw.PreferencesDialog

class (GObject o, O.IsDescendantOf CameraOptions o) => IsCameraOptions o

instance (GObject o, O.IsDescendantOf CameraOptions o) => IsCameraOptions o

toCameraOptions :: (MonadIO m, IsCameraOptions o) => o -> m CameraOptions
toCameraOptions = liftIO . unsafeCastTo CameraOptions

cameraOptionsClassInit :: GObjectClass -> IO ()
cameraOptionsClassInit klass = do
  withTransient @Gtk.WidgetClass (coerce klass) $ \widgetClass -> do
    Gtk.widgetClassSetTemplateFromResource widgetClass "/io/github/e_rk/ipcam/add-camera.ui"
    Gtk.widgetClassBindTemplateChildFull widgetClass "saveButton" False 0
    Gtk.widgetClassBindTemplateChildFull widgetClass "cancelButton" False 0
    Gtk.widgetClassBindTemplateChildFull widgetClass "cameraName" False 0
    Gtk.widgetClassBindTemplateChildFull widgetClass "cameraUri" False 0
    Gtk.widgetClassBindTemplateChildFull widgetClass "cameraPtzConfig" False 0

cameraOptionsInstanceInit ::
  GObjectClass ->
  CameraOptions ->
  IO CameraOptionsPrivate
cameraOptionsInstanceInit _klass cameraOptions = do
  Gtk.widgetInitTemplate cameraOptions

  optionsType <- glibType @CameraOptions
  saveButton <- Gtk.widgetGetTemplateChild cameraOptions optionsType "saveButton" >>= unsafeCastTo Gtk.Button
  cancelButton <- Gtk.widgetGetTemplateChild cameraOptions optionsType "cancelButton" >>= unsafeCastTo Gtk.Button
  nameInput <- Gtk.widgetGetTemplateChild cameraOptions optionsType "cameraName" >>= unsafeCastTo Adw.EntryRow
  uriInput <- Gtk.widgetGetTemplateChild cameraOptions optionsType "cameraUri" >>= unsafeCastTo Adw.EntryRow
  ptzConfig <- Gtk.widgetGetTemplateChild cameraOptions optionsType "cameraPtzConfig" >>= unsafeCastTo CameraPtzConfig

  _ <- Gtk.on cancelButton #clicked (addCameraDialogCancel cameraOptions)
  _ <- Gtk.on uriInput #changed uriInputValidate
  _ <- Gtk.on saveButton #clicked (saveWrapper cameraOptions)
  _ <- Gtk.on nameInput #changed (changedWrapper cameraOptions)

  saveAction <- newIORef Nothing

  return $
    CameraOptionsPrivate
      { saveButton = saveButton,
        cancelButton = cancelButton,
        nameInput = nameInput,
        uriInput = uriInput,
        ptzConfig = ptzConfig,
        editMode = AddCamera,
        handlers = saveAction
      }

addCameraDialogCancel :: (MonadIO m) => CameraOptions -> m ()
addCameraDialogCancel cameraOptions = do
  _ <- Adw.dialogClose cameraOptions
  pure ()

uriInputValidate :: (MonadIO m, ?self :: Adw.EntryRow) => m ()
uriInputValidate = do
  uri <- Gtk.get ?self #text
  let parsedUri = textToUri uri
  case parsedUri of
    Left err -> Gtk.widgetAddCssClass ?self "error" >> Gtk.set ?self [#tooltipText := err]
    _ -> Gtk.widgetRemoveCssClass ?self "error" >> Gtk.set ?self [#tooltipText := ""]

setCamera :: (MonadIO m) => CameraOptions -> Maybe Camera -> m ()
setCamera cameraOptions (Just camera) = do
  privData <- liftIO $ gobjectGetPrivateData cameraOptions
  liftIO $ gobjectSetPrivateData cameraOptions $ privData {editMode = EditCamera}
  Gtk.set privData.nameInput [#text := camera.name]
  Gtk.set privData.uriInput [#text := uriToText camera.videoURI]
  setPtzData' privData.ptzConfig camera.ptzActions
setCamera cameraOptions Nothing = do
  privData <- liftIO $ gobjectGetPrivateData cameraOptions
  Gtk.set privData.nameInput [#text := ""]
  Gtk.set privData.uriInput [#text := ""]
  setPtzData' privData.ptzConfig Nothing
  liftIO $ gobjectSetPrivateData cameraOptions $ privData {editMode = AddCamera}

getCamera :: (MonadIO m) => CameraOptions -> m (Maybe Camera)
getCamera cameraOptions = do
  privData <- liftIO $ gobjectGetPrivateData cameraOptions
  name <- Gtk.get privData.nameInput #text
  uri <- Gtk.get privData.uriInput #text
  ptzConfig <- getPtzData privData.ptzConfig
  let parsedUri = textToUri uri
  case parsedUri of
    Right u -> pure . Just $ Camera name u ptzConfig
    Left _ -> pure Nothing

saveWrapper :: (MonadIO m) => CameraOptions -> m ()
saveWrapper cameraOptions = do
  privData <- liftIO $ gobjectGetPrivateData cameraOptions
  handlers <- liftIO $ readIORef privData.handlers
  result <- case handlers of
    Just h -> do
      liftIO $ h.saveHandler cameraOptions privData.editMode
    Nothing -> pure False
  when result $ Adw.dialogClose cameraOptions >> pure ()

setHandlers :: (MonadIO m) => CameraOptions -> CameraOptionsHandlers -> m ()
setHandlers cameraOptions handlers = do
  privData <- liftIO $ gobjectGetPrivateData cameraOptions
  liftIO $ writeIORef privData.handlers (Just handlers)

changedWrapper :: (MonadIO m, ?self :: Adw.EntryRow) => CameraOptions -> m ()
changedWrapper cameraOptions = do
  privData <- liftIO $ gobjectGetPrivateData cameraOptions
  handlers <- liftIO $ readIORef privData.handlers
  name <- Gtk.get ?self #text
  result <- case handlers of
    Just h -> liftIO $ h.validateHandler name privData.editMode
    Nothing -> pure False
  case result of
    True -> do
      Gtk.widgetRemoveCssClass ?self "error"
      Gtk.set ?self [#tooltipText := ""]
    False -> do
      Gtk.widgetAddCssClass ?self "error"
      Gtk.set ?self [#tooltipText := "Camera with this name already exists."]
