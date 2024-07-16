module Storage
  ( newStorage,
    addCamera,
    getCameras,
    getCamerasList,
    getCamera,
    removeCamera,
    overwriteCamera,
    Storage,
  )
where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Map qualified as Map
import Data.Maybe
import Data.Text
import Data.Tuple
import Types

type StorageContext a = StateT CameraMap IO a

type Storage = MVar CameraMap

newStorage :: CameraMap -> IO (Storage)
newStorage = newMVar

withStorage :: (MonadIO m) => Storage -> StorageContext a -> m a
withStorage storage = liftIO . runner
  where
    runner action = modifyMVar storage (\x -> swap <$> runStateT action x)

addCamera_ :: Camera -> StorageContext ()
addCamera_ camera = do
  cameras <- get
  put $ Map.insert camera.name camera cameras

addCamera :: (MonadIO m) => Storage -> Camera -> m ()
addCamera storage camera = liftIO $ withStorage storage (addCamera_ camera)

getCameras_ :: StorageContext CameraMap
getCameras_ = get

getCameras :: (MonadIO m) => Storage -> m CameraMap
getCameras storage = liftIO $ withStorage storage getCameras_

getCamerasList_ :: StorageContext [Camera]
getCamerasList_ = getCameras_ >>= pure . Map.elems

getCamerasList :: (MonadIO m) => Storage -> m [Camera]
getCamerasList storage = liftIO $ withStorage storage getCamerasList_

getCamera_ :: Text -> StorageContext (Maybe Camera)
getCamera_ name = get >>= pure . Map.lookup name

getCamera :: (MonadIO m) => Storage -> Text -> m (Maybe Camera)
getCamera storage name = liftIO $ withStorage storage (getCamera_ name)

removeCamera_ :: Text -> StorageContext ()
removeCamera_ name = get >>= put . Map.delete name

removeCamera :: (MonadIO m) => Storage -> Text -> m ()
removeCamera storage name = liftIO $ withStorage storage (removeCamera_ name)

overwriteCamera_ :: Text -> Camera -> StorageContext Bool
overwriteCamera_ name camera = do
  existingCamera <- getCamera_ camera.name
  if (name == camera.name)
    then do
      addCamera_ camera
      pure True
    else
      if (isNothing existingCamera)
        then do
          removeCamera_ name
          addCamera_ camera
          pure True
        else pure False

overwriteCamera :: (MonadIO m) => Storage -> Text -> Camera -> m Bool
overwriteCamera storage name camera = liftIO $ withStorage storage (overwriteCamera_ name camera)
