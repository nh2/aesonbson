{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.AesonBson.Instances () where

import           Data.Bson as BSON
import           Data.Aeson.Types as AESON

import           Data.AesonBson


instance ToJSON BSON.Value where
  toJSON = aesonifyValue

instance ToJSON BSON.Document where
  toJSON = Object . aesonify
