{-# LANGUAGE OverloadedStrings #-}

module ModelSpec where

import Test.Hspec
import Model

import Servant
import Data.Map

shouldBeErrorWithCode (Left err) code = errHTTPCode err `shouldBe` code

spec = describe "Model" $ do
  describe "validateContact" $ do
    it "returns a contact when the values are valid" $ do
      let contact = Contact { contactEmail = "user@example.com"
                            , contactPhoneNumber = "5555555555"
                            , contactDetails = empty
                            }
      validateContact contact `shouldBe` Right contact

    it "returns an error when something is invalid" $ do
      let contact = Contact { contactEmail = "not a real email"
                            , contactPhoneNumber = "not a real number"
                            , contactDetails = empty
                            }
      validateContact contact `shouldBeErrorWithCode` 400

  describe "validateEmail" $ do
    it "returns the provided email when it is valid" $ do
      let email = "user@example.com"
      validateEmail email `shouldBe` Right email

    it "returns an error when the at sign is missing" $
      validateEmail "example.com" `shouldBeErrorWithCode` 400

    it "returns an error when the dot suffix is missing" $
      validateEmail "user@example" `shouldBeErrorWithCode` 400

  describe "validatePhoneNumber" $ do
    it "returns the phone number when it is valid and is only numeric" $ do
      let phoneNumber10 = "5555555555"
      let phoneNumber11 = "15555555555"
      validatePhoneNumber phoneNumber10 `shouldBe` Right phoneNumber10
      validatePhoneNumber phoneNumber11 `shouldBe` Right phoneNumber11

    it "strips non-numeric characters from valid phone numbers" $ do
      validatePhoneNumber "(555) 555-5555" `shouldBe` Right "5555555555"
      validatePhoneNumber "+1 (555) 555-5555" `shouldBe` Right "15555555555"

    it "returns an error if the number has the wrong number of digits" $
      mapM_ (\num -> validatePhoneNumber num `shouldBeErrorWithCode` 400)
        [ ""
        , "555"
        , "555555555"
        , "555555555555"
        , "five five five"
        ]
