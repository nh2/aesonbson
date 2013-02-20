{-# LANGUAGE OverloadedStrings #-}

module Data.AesonBson (
  aesonify, aesonifyValue,
  bsonify, bsonifyValue
) where

import           Data.Bson as BSON
import           Data.Aeson.Types as AESON
import qualified Data.Attoparsec.Number as Atto
import           Data.Monoid
import qualified Data.HashMap.Strict as HashMap (fromList, toList)
import qualified Data.Vector as Vector (fromList, toList)
import           Numeric (showHex)


bsonifyValue :: AESON.Value -> BSON.Value
bsonifyValue (Object obj) = Doc $ bsonify obj
bsonifyValue (AESON.Array array) = BSON.Array . map bsonifyValue . Vector.toList $ array
bsonifyValue (AESON.String str) = BSON.String str
bsonifyValue (Number n) = case n of { Atto.I int   -> Int64 $ fromIntegral int
                                    ; Atto.D float -> Float float }
bsonifyValue (AESON.Bool b) = BSON.Bool b
bsonifyValue (AESON.Null) = BSON.Null

aesonifyValue :: BSON.Value -> AESON.Value
aesonifyValue (Float f) = toJSON f
aesonifyValue (BSON.String s) = toJSON s
aesonifyValue (Doc doc) = Object $ aesonify doc
aesonifyValue (BSON.Array list) = AESON.Array . Vector.fromList $ map aesonifyValue list
aesonifyValue (Bin (Binary binary)) = toJSON binary
aesonifyValue (Fun (Function function)) = toJSON function
aesonifyValue (Uuid (UUID uuid)) = toJSON uuid
aesonifyValue (Md5 (MD5 md5)) = toJSON md5
aesonifyValue (UserDef (UserDefined userdef)) = toJSON userdef
aesonifyValue (ObjId (Oid w32 w64)) = toJSON $ showHex w32 (showHex w64 "")
aesonifyValue (BSON.Bool bool) = toJSON bool
aesonifyValue (UTC utc) = toJSON utc
aesonifyValue (BSON.Null) = AESON.Null
aesonifyValue (RegEx (Regex pattern mods)) = toJSON $ mconcat ["/", pattern, "/", mods]
aesonifyValue (JavaScr (Javascript env code)) = object [ "environment" .= aesonify env
                                                       , "code" .= code ]
aesonifyValue (Sym (Symbol sym)) = toJSON sym
aesonifyValue (Int32 int32) = toJSON int32
aesonifyValue (Int64 int64) = toJSON int64
aesonifyValue (Stamp (MongoStamp int64)) = toJSON int64
aesonifyValue (MinMax mm) = case mm of { MinKey -> toJSON (-1 :: Int)
                                       ; MaxKey -> toJSON (1 :: Int)}


bsonify :: AESON.Object -> BSON.Document
bsonify = map (\(t, v) -> t := bsonifyValue v) . HashMap.toList

aesonify :: BSON.Document -> AESON.Object
aesonify = HashMap.fromList . map (\(l := v) -> (l, aesonifyValue v))
