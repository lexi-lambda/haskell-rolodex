{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Contact where

import Control.Monad.Trans.Either
import Data.Int
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite
import Servant

import qualified Data.Text as T

import DB
import Model
import Util

type ContactsAPI = Get '[JSON] [Entity Contact]
              :<|> ReqBody '[JSON] Contact :> Post '[] ()
              :<|> "search" :> QueryParam "query" Text
                            :> Get '[JSON] [Entity Contact]
              :<|> Capture "id" Int64 :>
                   ( Get '[JSON] (Entity Contact)
                :<|> ReqBody '[JSON] Contact :> Put '[] ()
                :<|> Delete '[] ()
                   )

contactsServer :: Server ContactsAPI
contactsServer = listContacts
            :<|> createContact
            :<|> searchContacts
            :<|> contactOperations
  where contactOperations contactId = getContact contactId
                                 :<|> updateContact contactId
                                 :<|> deleteContact contactId

-- GET /
listContacts :: EitherT ServantErr IO [Entity Contact]
listContacts = runDB $ selectList [] []

-- POST /
createContact :: Contact -> EitherT ServantErr IO ()
createContact contact = do
  validated <- hoistEither $ validateContact contact
  insertion <- runDB $ insertBy validated
  case insertion of
    (Right _) -> return ()
    (Left  _) -> left err400
      { errBody = "That phone number or email already exists in the database" }

-- GET /:id
getContact :: Int64 -> EitherT ServantErr IO (Entity Contact)
getContact contactId = getById contactId >>= maybeTo404 "contact"

-- PUT /:id
updateContact :: Int64 -> Contact -> EitherT ServantErr IO ()
updateContact contactId contact = do
    validated <- hoistEither $ validateContact contact
    replacement <- runDB $ replaceBy key validated
    case replacement of
      (Right _) -> return ()
      (Left  _) -> left err400 { errBody = "No contact exists with that id" }
  where key = toSqlKey contactId

-- DELETE /:id
deleteContact :: Int64 -> EitherT ServantErr IO ()
deleteContact contactId = runDB $ delete key
  where key = toSqlKey contactId :: (Key Contact)

-- GET /search?query
searchContacts :: Maybe Text -> EitherT ServantErr IO [Entity Contact]
searchContacts param = do
  query <- requireQueryParam "query" param
  let sql = unlines [ "SELECT ?? FROM contact"
                    , "WHERE email        LIKE '%' || ? || '%'"
                    , "   OR phone_number LIKE '%' || ? || '%'"
                    , "   OR details      LIKE '%' || ? || '%'"
                    ]
  runDB $ rawSql (T.pack sql) (replicate 3 $ toPersistValue query)
