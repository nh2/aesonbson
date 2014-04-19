{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Hspec
import           Data.Aeson.Types as AESON
import           Data.Bson as BSON
import qualified Data.Text as T
import qualified Data.Scientific as Scientific

import Data.AesonBson

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
    it "should converts floats well" $ do
      (bsonifyValue $ AESON.Number $ Scientific.scientific 5 (-1))   `shouldBe` BSON.Float 0.5
      (bsonifyValue $ AESON.Number $ Scientific.scientific 5 (0))    `shouldBe` BSON.Int64 5
      (bsonifyValue $ AESON.Number $ Scientific.scientific (-5) (0)) `shouldBe` BSON.Int64 (-5)
