{-# LANGUAGE TypeSynonymInstances #-}

module Data.AesonBson (
  aesonify, aesonifyValue,
  bsonify, bsonifyValue
) where

import Data.Bson as BSON
import Data.Aeson.Types as AESON
import Data.Attoparsec.Number as Atto
import Data.CompactString.UTF8 as UTF8 hiding (map)
import Data.Text as Text hiding (map)
import Data.Map as Map (fromList, toList)
import Data.Vector as Vector (toList)
import Numeric

instance ToJSON BSON.Value where
  toJSON = aesonifyValue

instance ToJSON Document where
  toJSON = Object . aesonify

instance ToJSON UString where
  toJSON = AESON.String . ustringToText

bsonifyValue :: AESON.Value -> BSON.Value
bsonifyValue (Object obj) = Doc $ bsonify obj
bsonifyValue (AESON.Array array) = BSON.Array . map bsonifyValue . Vector.toList $ array
bsonifyValue (AESON.String str) = BSON.String $ textToUstring str
bsonifyValue (Number n) = case n of { I int   -> Int64 $ fromIntegral int
                                    ; D float -> Float float }
bsonifyValue (AESON.Bool b) = BSON.Bool b
bsonifyValue (AESON.Null) = BSON.Null

aesonifyValue :: BSON.Value -> AESON.Value
aesonifyValue (Float f) = toJSON f
aesonifyValue (BSON.String s) = toJSON s
aesonifyValue (Doc doc) = toJSON doc
aesonifyValue (BSON.Array list) = toJSON list
aesonifyValue (Bin (Binary binary)) = toJSON binary
aesonifyValue (Fun (Function function)) = toJSON function
aesonifyValue (Uuid (UUID uuid)) = toJSON uuid
aesonifyValue (Md5 (MD5 md5)) = toJSON md5
aesonifyValue (UserDef (UserDefined userdef)) = toJSON userdef
aesonifyValue (ObjId (Oid w32 w64)) = toJSON $ showHex w32 (showHex w64 "")
aesonifyValue (BSON.Bool bool) = toJSON bool
aesonifyValue (UTC utc) = toJSON utc
aesonifyValue (BSON.Null) = AESON.Null
aesonifyValue (RegEx (Regex pattern mods)) = toJSON $
                                           '/' : (UTF8.unpack pattern) ++
                                           '/' : (UTF8.unpack mods)
aesonifyValue (JavaScr (Javascript env code)) = toJSON . Map.fromList $
                                              [ (Text.pack "environment", toJSON env)
                                              , (Text.pack "code", toJSON code)]
aesonifyValue (Sym (Symbol sym)) = toJSON sym
aesonifyValue (Int32 int32) = toJSON int32
aesonifyValue (Int64 int64) = toJSON int64
aesonifyValue (Stamp (MongoStamp int64)) = toJSON int64
aesonifyValue (MinMax mm) = case mm of { MinKey -> toJSON (-1 :: Int)
                                       ; MaxKey -> toJSON (1 :: Int)}


bsonify :: AESON.Object -> BSON.Document
bsonify = map (\(t, v) -> (textToUstring t := bsonifyValue v)) . Map.toList

aesonify :: BSON.Document -> AESON.Object
aesonify = Map.fromList . map (\(l := v) -> (ustringToText l, aesonifyValue v))

ustringToText :: UString -> Text
ustringToText = Text.pack . UTF8.unpack

textToUstring :: Text -> UString
textToUstring = UTF8.pack . Text.unpack
