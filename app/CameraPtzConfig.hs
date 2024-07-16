{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module CameraPtzConfig
  ( CameraPtzConfig (..),
    IsCameraPtzConfig,
    toCameraPtzConfig,
    getPtzData,
    setPtzData',
    trimHeaderRows,
  )
where

import CameraActionRow
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.CaseInsensitive qualified as CI
import Data.Char (toLower)
import Data.Coerce (coerce)
import Data.GI.Base
  ( AttrOp ((:=)),
    GObject,
    ManagedPtr (..),
    TypedObject (glibType),
    unsafeCastTo,
    withTransient,
  )
import Data.GI.Base.Attributes (AttrOpTag (..))
import Data.GI.Base.GObject
  ( DerivedGObject (..),
    GObjectClass (..),
    gobjectGetPrivateData,
    gobjectSetPrivateData,
    registerGType,
  )
import Data.GI.Base.Overloading qualified as O
import Data.List (findIndex)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as T (Text, findIndex, null, pack, splitAt, uncons)
import Data.Text.Encoding qualified as T
import GI.Adw qualified as Adw
import GI.GObject qualified as Gobj
import GI.Gio.Interfaces qualified as Gio
import GI.Gtk qualified as Gtk
import Network.HTTP.Simple
import Types

newtype CameraPtzConfig = CameraPtzConfig (ManagedPtr CameraPtzConfig)

instance TypedObject CameraPtzConfig where
  glibType = registerGType CameraPtzConfig

instance GObject CameraPtzConfig

data CameraPtzConfigPrivate = CameraPtzConfigPrivate
  { headerGroup :: Adw.PreferencesGroup,
    headerRows :: [Adw.EntryRow],
    requestMethod :: Adw.ComboRow,
    actionRows :: Map.Map PtzCommand CameraActionRow
  }

instance DerivedGObject CameraPtzConfig where
  type GObjectParentType CameraPtzConfig = Adw.PreferencesPage
  type GObjectPrivateData CameraPtzConfig = CameraPtzConfigPrivate
  objectTypeName = "Ipcam-CameraPtzConfig"
  objectClassInit = cameraPtzConfigClassInit
  objectInstanceInit = cameraPtzConfigInstanceInit
  objectInterfaces = []

instance O.HasParentTypes CameraPtzConfig

type instance O.ParentTypes CameraPtzConfig = Adw.PreferencesPage ': O.ParentTypes Adw.PreferencesPage

class (GObject o, O.IsDescendantOf CameraPtzConfig o) => IsCameraPtzConfig o

instance (GObject o, O.IsDescendantOf CameraPtzConfig o) => IsCameraPtzConfig o

toCameraPtzConfig :: (MonadIO m, IsCameraPtzConfig o) => o -> m CameraPtzConfig
toCameraPtzConfig = liftIO . unsafeCastTo CameraPtzConfig

cameraPtzConfigClassInit :: GObjectClass -> IO ()
cameraPtzConfigClassInit klass = do
  withTransient @Gtk.WidgetClass (coerce klass) $ \widgetClass -> do
    Gtk.widgetClassSetTemplateFromResource widgetClass "/io/github/e_rk/ipcam/camera-ptz-config.ui"
    Gtk.widgetClassBindTemplateChildFull widgetClass "headerAddButton" False 0
    Gtk.widgetClassBindTemplateChildFull widgetClass "headerGroup" False 0
    Gtk.widgetClassBindTemplateChildFull widgetClass "requestMethod" False 0
    mapM_ (\x -> Gtk.widgetClassBindTemplateChildFull widgetClass x False 0) (snd <$> ptzActionAndName)
  pure ()

cameraPtzConfigInstanceInit ::
  GObjectClass ->
  CameraPtzConfig ->
  IO CameraPtzConfigPrivate
cameraPtzConfigInstanceInit _klass cameraPtzConfig = do
  Gtk.widgetInitTemplate cameraPtzConfig

  uriRowType <- glibType @CameraPtzConfig
  headerAddButton <- Gtk.widgetGetTemplateChild cameraPtzConfig uriRowType "headerAddButton" >>= unsafeCastTo Gtk.Button
  headerGroup <- Gtk.widgetGetTemplateChild cameraPtzConfig uriRowType "headerGroup" >>= unsafeCastTo Adw.PreferencesGroup
  requestMethod <- Gtk.widgetGetTemplateChild cameraPtzConfig uriRowType "requestMethod" >>= unsafeCastTo Adw.ComboRow
  actionRows <- mapM (\x -> (,) (fst x) <$> (Gtk.widgetGetTemplateChild cameraPtzConfig uriRowType (snd x) >>= unsafeCastTo CameraActionRow)) ptzActionAndName

  _ <- Gtk.on headerAddButton #clicked (addActionRow cameraPtzConfig)

  return $
    CameraPtzConfigPrivate
      { headerGroup = headerGroup,
        headerRows = mempty,
        requestMethod = requestMethod,
        actionRows = Map.fromList actionRows
      }

addActionRow :: (MonadIO m) => CameraPtzConfig -> m ()
addActionRow cameraPtzConfig = do
  startRow <- Adw.new Adw.EntryRow [#title := "HTTP header"]
  privData <- liftIO $ gobjectGetPrivateData cameraPtzConfig
  Adw.preferencesGroupAdd privData.headerGroup startRow
  let next =
        let prev = privData.headerRows
            dreq = startRow
         in dreq : prev
  liftIO $ gobjectSetPrivateData cameraPtzConfig $ privData {headerRows = next}

initAndInsertActionRow :: (MonadIO m) => CameraPtzConfig -> [AttrOp Adw.EntryRow AttrConstruct] -> m Adw.EntryRow
initAndInsertActionRow cameraPtzConfig initProperties = do
  privData <- liftIO $ gobjectGetPrivateData cameraPtzConfig
  row <- Adw.new Adw.EntryRow initProperties
  Adw.preferencesGroupAdd privData.headerGroup row
  pure row

parseHeader :: T.Text -> Maybe Header
parseHeader input = do
  idx <- T.findIndex (== '=') input
  (header, value) <- pure $ T.splitAt idx input
  stripped <- snd <$> T.uncons value
  pure $ (CI.mk $ T.encodeUtf8 header, T.encodeUtf8 stripped)

headerToText :: Header -> T.Text
headerToText (header, value) =
  let utf8header = T.decodeUtf8 $ CI.original header
      utf8value = T.decodeUtf8 value
   in utf8header <> "=" <> utf8value

getPtzData :: (MonadIO m) => CameraPtzConfig -> m (Maybe PtzData)
getPtzData cameraPtzConfig = do
  privData <- liftIO $ gobjectGetPrivateData cameraPtzConfig
  headers <- sequenceA <$> mapM (\x -> parseHeader <$> Gtk.get x #text) privData.headerRows
  actions <- sequenceA <$> mapM getPtzRequest privData.actionRows
  selected <- Adw.comboRowGetSelected privData.requestMethod
  model <- fromJust <$> Adw.comboRowGetModel privData.requestMethod
  item <- Gio.listModelGetItem model selected >>= liftIO . Gobj.unsafeCastTo Gtk.StringObject . fromJust
  requestMethodString <- Gtk.get item #string
  pure $ PtzData <$> headers <*> (Just requestMethodString) <*> actions

setPtzData' :: (MonadIO m) => CameraPtzConfig -> Maybe PtzData -> m ()
setPtzData' cameraPtzConfig (Just request) = do
  privData <- liftIO $ gobjectGetPrivateData cameraPtzConfig
  let actionMap = Map.toList privData.actionRows
  mapM_ (Adw.preferencesGroupRemove privData.headerGroup) privData.headerRows
  mapM_ (\x -> setPtzRequest (snd x) (Map.lookup (fst x) request.requestCommands)) actionMap
  model <- fromJust <$> Adw.comboRowGetModel privData.requestMethod >>= liftIO . Gobj.unsafeCastTo Gtk.StringList
  n <- Gtk.get model #nItems
  idx <- findIndex (any (== request.requestMethod)) <$> mapM (\x -> Gtk.stringListGetString model x) [0 .. n]
  case idx of
    Just x -> Adw.comboRowSetSelected privData.requestMethod (fromIntegral x)
    Nothing -> pure ()
  rows <- mapM (\x -> initAndInsertActionRow cameraPtzConfig [#title := "HTTP header", #text := headerToText x]) request.requestHeaders
  liftIO $ gobjectSetPrivateData cameraPtzConfig privData {headerRows = rows}
setPtzData' cameraPtzConfig Nothing = do
  privData <- liftIO $ gobjectGetPrivateData cameraPtzConfig
  mapM_ (Adw.preferencesGroupRemove privData.headerGroup) privData.headerRows
  mapM_ (\x -> setPtzRequest x Nothing) privData.actionRows
  liftIO $ gobjectSetPrivateData cameraPtzConfig privData {headerRows = mempty}

trimHeaderRows :: (MonadIO m) => CameraPtzConfig -> m ()
trimHeaderRows cameraPtzConfig = do
  privData <- liftIO $ gobjectGetPrivateData cameraPtzConfig
  (emptyRows, filledRows) <- partitionM (\x -> T.null <$> Gtk.get x #text) privData.headerRows
  mapM_ (Adw.preferencesGroupRemove privData.headerGroup) emptyRows
  liftIO $ gobjectSetPrivateData cameraPtzConfig privData {headerRows = filledRows}

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = pure ([], [])
partitionM f (x : xs) = do
  res <- f x
  (as, bs) <- partitionM f xs
  pure ([x | res] ++ as, [x | not res] ++ bs)

camelCase :: String -> String
camelCase [] = []
camelCase (x : xs) = toLower x : xs

commandToId :: PtzCommand -> T.Text
commandToId = T.pack . camelCase . show

ptzActionsList :: [PtzCommand]
ptzActionsList = [minBound .. maxBound]

ptzActionAndName :: [(PtzCommand, T.Text)]
ptzActionAndName = [(x, commandToId x) | x <- ptzActionsList]
