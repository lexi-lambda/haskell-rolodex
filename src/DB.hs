{-# LANGUAGE FlexibleContexts #-}

module DB where

import Control.Monad.Logger
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Text (pack)
import Database.Persist.Sqlite
import System.Environment

runDB :: (MonadBaseControl IO m, MonadIO m)
      => (SqlPersistT (NoLoggingT (ResourceT m))) a -> m a
runDB actions = do
  filename <- liftIO $ getEnv "SQLITE_FILENAME"
  runSqlite (pack filename) actions
