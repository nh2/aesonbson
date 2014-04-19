{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Aeson.Types as AESON
import           Data.Bson as BSON
import           Data.Int
import qualified Data.Text as T
import qualified Data.Scientific as Scientific

import           Data.AesonBson

main :: IO ()
main = hspec $ do
  describe "BSON -> JSON" $ do

    it "converts an ObjId to 24 digits hex" $ do
      -- https://github.com/nh2/aesonbson/pull/2
      let objid = ObjId (read "000000010000000000000001" :: ObjectId)
          AESON.String str = aesonifyValue objid
      str `shouldBe` "000000010000000000000001"
      T.length str `shouldBe` 24

  describe "JSON -> BSON" $ do
    it "converts Int32 max bound + 1 to Int64" $ do
      let x = succ $ fromIntegral (maxBound :: Int32) :: Integer
      (bsonifyValue . AESON.Number $ Scientific.scientific x 0)
        `shouldBe` BSON.Int64 (fromIntegral x)

    it "converts Int32 max bound to Int32" $ do
      let x = fromIntegral (maxBound :: Int32) :: Integer
      (bsonifyValue . AESON.Number $ Scientific.scientific x 0)
        `shouldBe` BSON.Int32 (fromIntegral x)

    it "converts Int32 min bound to Int32" $ do
      let x = fromIntegral (minBound :: Int32) :: Integer
      (bsonifyValue . AESON.Number $ Scientific.scientific x 0)
        `shouldBe` BSON.Int32 (fromIntegral x)

    it "converts Int32 min bound - 1 to Int64" $ do
      let x = pred $ fromIntegral (minBound :: Int32) :: Integer
      (bsonifyValue . AESON.Number $ Scientific.scientific x 0)
        `shouldBe` BSON.Int64 (fromIntegral x)

    it "converts number smaller than Int32 min bound to Int64" $ do
      let x = fromIntegral (minBound :: Int32) :: Integer
      (bsonifyValue . AESON.Number $ Scientific.scientific (pred x) 0)
        `shouldBe` BSON.Int64 (fromIntegral $ pred x)

    it "converts Int32 to Int32" $ property $ \(x :: Int32) ->
      (bsonifyValue . AESON.Number $ Scientific.scientific (fromIntegral x) 0)
        `shouldBe` BSON.Int32 x
