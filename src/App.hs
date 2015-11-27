{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Database.Persist.Sqlite
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import API.Contact
import DB
import Model

type ServerAPI = "contacts" :> ContactsAPI

server :: Server ServerAPI
server = contactsServer

app :: Application
app = serve (Proxy :: (Proxy ServerAPI)) server

main :: IO ()
main = do
  runDB $ runMigration migrateAll
  run 3000 app
