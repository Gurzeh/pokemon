{-# LANGUAGE OverloadedStrings #-}
module Pokemon.EncryptSpec where

import qualified Data.ByteString as BS
import           System.Random   (randomIO)
import           Test.Hspec
import           Test.QuickCheck

import qualified Pokemon.Encrypt as Encrypt


spec :: Spec
spec = do
  describe "encrypt" $
    it "is a pure function" $
      property $ \iv input -> do
        encrypted1 <- Encrypt.encryptIO iv input
        encrypted2 <- Encrypt.encryptIO iv input
        encrypted1 `shouldBe` encrypted2

  describe "random" $
    it "generates 32 byte IVs" $ do
      iv <- randomIO
      BS.length (Encrypt.ivToBS iv) `shouldBe` Encrypt.expectedIvSize


  describe "xxHash32" $ do
    it "is a pure function" $
      property $ \seed input -> do
        hash1 <- Encrypt.xxHash32IO seed input
        hash2 <- Encrypt.xxHash32IO seed input
        hash1 `shouldBe` hash2

    it "does the same as the Python version" $ do
      hash <- Encrypt.xxHash32IO 0x1B845238 "abcde"
      hash `shouldBe` 327543353


  describe "xxHash64" $ do
    it "is a pure function" $
      property $ \seed input -> do
        hash1 <- Encrypt.xxHash64IO seed input
        hash2 <- Encrypt.xxHash64IO seed input
        hash1 `shouldBe` hash2
