{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Util where

import Control.Monad
import Control.Monad.Trans.Control
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Data.Int
import Data.String
import Database.Persist.Sqlite
import Servant

import qualified Data.ByteString.Lazy as B

import DB

-- |Fetches a record given an 'Int64' id and wraps it in an 'Entity'.
getById :: (MonadIO m, MonadBaseControl IO m, ToBackendKey SqlBackend record)
        => Int64 -> m (Maybe (Entity record))
getById contactId = runDB $ boxEntity (get key)
  where key = toSqlKey contactId
        boxEntity = liftM $ fmap (Entity key)

-- |Adapts 'Maybe' values representing potentially-existing records to use
-- 'EitherT'’s error-handling machinery. Produces a Servant error upon failure.
maybeTo404 :: Monad m => B.ByteString -> Maybe a -> EitherT ServantErr m a
maybeTo404 _          (Just record) = return record
maybeTo404 recordName Nothing       = left err
  where err = err404
          { errBody = B.concat ["No ", recordName, " exists for that id"] }

-- |Enforces the existence of a textual query param. Checks that it both exists
-- and is non-empty.
requireQueryParam :: (Eq a, Monad m, IsString a)
                  => B.ByteString -> Maybe a -> EitherT ServantErr m a
requireQueryParam name Nothing = left err400
  { errBody = B.concat [ "The '", name, "' parameter must be provided" ] }
requireQueryParam name (Just "") = left err400
  { errBody = B.concat [ "The '", name, "' parameter cannot be empty" ] }
requireQueryParam _ (Just value) = return value
