<<<<<<< HEAD

module Main where

import Test.Framework
import Test.Framework.Providers.HUnit

import Test.HUnit hiding (Test)

import Data.List


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
        testGroup "Sorting Group 2" [
                testCase "sort7" test_sort7
            ]
    ]


test_sort7 :: Assertion
test_sort7 = sort [8, 7, 2, 5, 4, 9, 6, 1, 0, 3] @?= [0..9]
=======
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Hspec
import           Data.Aeson.Types as AESON
import           Data.Bson as BSON
import qualified Data.Text as T

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
>>>>>>> 7f7e6c3733de36cf5c82c5ccbacb6f18466a2c17
