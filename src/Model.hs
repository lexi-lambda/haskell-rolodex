{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model where

import Data.Char
import Data.Map.Lazy (Map)
import Data.Text (Text)
import Database.Persist.TH
import GHC.Generics
import Servant
import Text.Regex

import qualified Data.Text as T

type ContactDetails = Map Text Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Contact json
  email Text
  phoneNumber Text
  details ContactDetails

  UniqueEmail email
  UniquePhoneNumber phoneNumber

  deriving Eq Show Generic
|]

validateContact :: Contact -> Either ServantErr Contact
validateContact contact = do
  email <- validateEmail (contactEmail contact)
  phoneNumber <- validatePhoneNumber (contactPhoneNumber contact)
  return contact { contactEmail = email, contactPhoneNumber = phoneNumber }

validateEmail :: Text -> Either ServantErr Text
validateEmail email = case match of
    (Just _) -> Right email
    Nothing  -> Left err400 { errBody = "Email address is invalid" }
  where regex = mkRegex "^.+@.+\\..+$"
        match = matchRegex regex (T.unpack email)

validatePhoneNumber :: Text -> Either ServantErr Text
validatePhoneNumber phoneNumber = case T.length digits of
    10 -> Right digits
    11 -> Right digits
    _  -> Left err400 { errBody = "Phone number must contain 10 or 11 digits" }
  where digits = T.filter isDigit phoneNumber
