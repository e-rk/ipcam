{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module CameraActionRow
  ( CameraActionRow (..),
    IsCameraActionRow,
    toCameraActionRow,
    getPtzRequest,
    setPtzRequest,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe
import Data.Coerce (coerce)
import Data.GI.Base
  ( AttrOp ((:=)),
    GObject,
    ManagedPtr (..),
    TypedObject (glibType),
    unsafeCastTo,
    withTransient,
  )
import Data.GI.Base.GObject
  ( DerivedGObject (..),
    GObjectClass (..),
    gobjectGetPrivateData,
    registerGType,
  )
import Data.GI.Base.Overloading qualified as O
import Data.Text qualified as T (Text)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import Types

newtype CameraActionRow = CameraActionRow (ManagedPtr CameraActionRow)

instance TypedObject CameraActionRow where
  glibType = registerGType CameraActionRow

instance GObject CameraActionRow

data CameraActionRowPrivate
  = CameraActionRowPrivate
  { uriRow :: Adw.EntryRow,
    startRow :: Adw.EntryRow,
    stopRow :: Adw.EntryRow
  }

instance DerivedGObject CameraActionRow where
  type GObjectParentType CameraActionRow = Adw.ExpanderRow
  type GObjectPrivateData CameraActionRow = CameraActionRowPrivate
  objectTypeName = "Ipcam-CameraActionRow"
  objectClassInit = cameraActionRowClassInit
  objectInstanceInit = cameraActionRowInstanceInit
  objectInterfaces = []

instance O.HasParentTypes CameraActionRow

type instance O.ParentTypes CameraActionRow = Adw.ExpanderRow ': O.ParentTypes Adw.ExpanderRow

class (GObject o, O.IsDescendantOf CameraActionRow o) => IsCameraActionRow o

instance (GObject o, O.IsDescendantOf CameraActionRow o) => IsCameraActionRow o

toCameraActionRow :: (MonadIO m, IsCameraActionRow o) => o -> m CameraActionRow
toCameraActionRow = liftIO . unsafeCastTo CameraActionRow

cameraActionRowClassInit :: GObjectClass -> IO ()
cameraActionRowClassInit klass = do
  withTransient @Gtk.WidgetClass (coerce klass) $ \widgetClass -> do
    Gtk.widgetClassSetTemplateFromResource widgetClass "/io/github/e_rk/ipcam/add-action.ui"
    Gtk.widgetClassBindTemplateChildFull widgetClass "requestUri" False 0
    Gtk.widgetClassBindTemplateChildFull widgetClass "requestStartData" False 0
    Gtk.widgetClassBindTemplateChildFull widgetClass "requestStopData" False 0
  pure ()

cameraActionRowInstanceInit ::
  GObjectClass ->
  CameraActionRow ->
  IO CameraActionRowPrivate
cameraActionRowInstanceInit _klass cameraActionRow = do
  Gtk.widgetInitTemplate cameraActionRow

  uriRowType <- glibType @CameraActionRow
  uriRow <- Gtk.widgetGetTemplateChild cameraActionRow uriRowType "requestUri" >>= unsafeCastTo Adw.EntryRow
  startRow <- Gtk.widgetGetTemplateChild cameraActionRow uriRowType "requestStartData" >>= unsafeCastTo Adw.EntryRow
  stopRow <- Gtk.widgetGetTemplateChild cameraActionRow uriRowType "requestStopData" >>= unsafeCastTo Adw.EntryRow

  _ <- Gtk.on uriRow #changed (validateUri uriRow)

  return $
    CameraActionRowPrivate
      { uriRow = uriRow,
        startRow = startRow,
        stopRow = stopRow
      }

validateUri :: (MonadIO m) => Adw.EntryRow -> m ()
validateUri row = do
  requestUri <- textToUri <$> Gtk.get row #text
  rowErrorClass requestUri row

rowErrorClass :: (MonadIO m) => Either T.Text a -> Adw.EntryRow -> m ()
rowErrorClass (Left err) widget = Gtk.widgetAddCssClass widget "error" >> Gtk.set widget [#tooltipText := err]
rowErrorClass (Right _) widget = Gtk.widgetRemoveCssClass widget "error" >> Gtk.set widget [#tooltipText := ""]

getPtzRequest :: (MonadIO m) => CameraActionRow -> m (Maybe PtzRequest)
getPtzRequest cameraActionRow = do
  privData <- liftIO $ gobjectGetPrivateData cameraActionRow
  requestUri <- textToUri <$> Gtk.get privData.uriRow #text
  rowErrorClass requestUri privData.uriRow
  requestStartData <- Gtk.get privData.startRow #text
  requestStopData <- Gtk.get privData.stopRow #text
  runMaybeT $ do
    uri <- hoistMaybe $ rightToMaybe requestUri
    pure $ PtzRequest uri requestStartData requestStopData

setPtzRequest :: (MonadIO m) => CameraActionRow -> Maybe PtzRequest -> m ()
setPtzRequest cameraActionRow (Just request) = do
  privData <- liftIO $ gobjectGetPrivateData cameraActionRow
  Gtk.set privData.uriRow [#text := uriToText request.requestURI]
  Gtk.set privData.startRow [#text := request.requestStartBody]
  Gtk.set privData.stopRow [#text := request.requestStopBody]
setPtzRequest cameraActionRow Nothing = do
  privData <- liftIO $ gobjectGetPrivateData cameraActionRow
  Gtk.set privData.uriRow [#text := ""]
  Gtk.set privData.startRow [#text := ""]
  Gtk.set privData.stopRow [#text := ""]
