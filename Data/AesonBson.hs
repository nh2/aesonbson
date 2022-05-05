{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Convert JSON to BSON and the other way around.
--
-- Note that BSON has more data types than JSON,
-- so some BSON to JSON conversions are not bijective and somewhat arbitrary.
--
-- This means that for some BSON objects:
--
-- >bsonify . aesonify /= id
-- >bsonifyValue . aesonifyValue /= id
--
-- We tried to choose sensible translations on those cases.
module Data.AesonBson (
  aesonify, aesonifyValue,
  bsonify, bsonifyValue,
  bsonifyError, bsonifyBound,
  errorRange, bound,
) where

-- TODO Document the arbitrary choices in the Haddock.

import           Data.Bson as BSON
import           Data.Aeson.Types as AESON
import           Data.Int
import qualified Data.Scientific as S
import qualified Data.Text as Text
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vector (fromList, toList)

# if MIN_VERSION_aeson(2, 0, 0)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as HashMap (fromList, toList)
textToKey :: Text.Text -> AESON.Key
textToKey = Key.fromText
keyToText :: AESON.Key -> Text.Text
keyToText = Key.toText
# else
import qualified Data.HashMap.Strict as HashMap (fromList, toList)
textToKey :: Text.Text -> Text.Text
textToKey = id
keyToText :: Text.Text -> Text.Text
keyToText = id
# endif

-- | Converts an AESON object to a BSON document. Will yeld an error for JSON numbers that are too big.
bsonifyError :: AESON.Object -> BSON.Document
bsonifyError = bsonify errorRange

-- | Converts an AESON object to a BSON document. Will bound JSON numbers that are too big.
bsonifyBound :: AESON.Object -> BSON.Document
bsonifyBound = bsonify bound

-- | Converts an AESON object to a BSON document. The user can provide a function to deal with JSON numbers that are too big.
bsonify :: (S.Scientific -> BSON.Value) -> AESON.Object -> BSON.Document
bsonify f o = map (\(t, v) -> keyToText t := bsonifyValue f v) $ HashMap.toList o

-- | Converts a BSON document to an AESON object.
aesonify :: BSON.Document -> AESON.Object
aesonify = HashMap.fromList . map (\(l := v) -> (textToKey l, aesonifyValue v))


-- | Helpers

-- | Converts a JSON value to BSON.
bsonifyValue :: (S.Scientific -> BSON.Value) -> AESON.Value -> BSON.Value
bsonifyValue f (Object obj)        = Doc $ bsonify f obj
bsonifyValue f (AESON.Array array) = BSON.Array . map (bsonifyValue f) . Vector.toList $ array
bsonifyValue _ (AESON.String str)  = BSON.String str
bsonifyValue _ (AESON.Bool b)      = BSON.Bool b
bsonifyValue _ (AESON.Null)        = BSON.Null
bsonifyValue f (AESON.Number n)    = f n 


-- | Converts a BSON value to JSON.
aesonifyValue :: BSON.Value -> AESON.Value
aesonifyValue (Float f)                       = toJSON f
aesonifyValue (BSON.String s)                 = toJSON s
aesonifyValue (Doc doc)                       = Object $ aesonify doc
aesonifyValue (BSON.Array list)               = AESON.Array . Vector.fromList $ map aesonifyValue list
aesonifyValue (Bin (Binary binary))           = toJSON $ T.decodeUtf8 binary
aesonifyValue (Fun (Function function))       = toJSON $ T.decodeUtf8 function
aesonifyValue (Uuid (UUID uuid))              = toJSON $ T.decodeUtf8 uuid
aesonifyValue (Md5 (MD5 md5))                 = toJSON $ T.decodeUtf8 md5
aesonifyValue (UserDef (UserDefined userdef)) = toJSON $ T.decodeUtf8 userdef
aesonifyValue (ObjId oid)                     = toJSON $ show oid -- Relies on bson to show the OID as 24 digit hex. It would be better if BSON exposed a non-show function for this, preferably a fast one.
aesonifyValue (BSON.Bool bool)                = toJSON bool
aesonifyValue (UTC utc)                       = toJSON utc
aesonifyValue (BSON.Null)                     = AESON.Null
aesonifyValue (RegEx (Regex pattern mods))    = toJSON $ mconcat ["/", pattern, "/", mods]
aesonifyValue (JavaScr (Javascript env code)) = object [ "environment" .= aesonify env, "code" .= code ]
aesonifyValue (Sym (Symbol sym))              = toJSON sym
aesonifyValue (Int32 int32)                   = toJSON int32
aesonifyValue (Int64 int64)                   = toJSON int64
aesonifyValue (Stamp (MongoStamp int64))      = toJSON int64
aesonifyValue (MinMax mm)                     = case mm of { MinKey -> toJSON (-1 :: Int)
                                                           ; MaxKey -> toJSON (1 :: Int)}

int64MaxBound, int32MaxBound, int64MinBound, int32MinBound :: S.Scientific
int64MaxBound  = toScientific (maxBound :: Int64)
int32MaxBound  = toScientific (maxBound :: Int32)
int64MinBound  = toScientific (minBound :: Int64)
int32MinBound  = toScientific (minBound :: Int32)

toScientific :: Integral i => i -> S.Scientific
toScientific i = S.scientific (fromIntegral i :: Integer ) 0

expo :: S.Scientific -> Int
expo n  = S.base10Exponent n

coef :: S.Scientific -> Integer
coef n = S.coefficient n

-- Error when the number of out of range
errorRange :: S.Scientific -> BSON.Value
errorRange n | n < int64MinBound = error $ "Number out of min range: " ++ (show n)
errorRange n | n > int64MaxBound = error $ "Number out of max range: " ++ (show n)
errorRange n = bsonifyNumberInRange n 

-- Bound the number when out of range.
bound :: S.Scientific -> BSON.Value
bound n | n < int64MinBound = Int64 minBound
bound n | n > int64MaxBound = Int64 maxBound
bound n = bsonifyNumberInRange n 

-- Function for converting numbers within range; int64MinBound < n < int64MaxBound
bsonifyNumberInRange :: S.Scientific -> BSON.Value 
bsonifyNumberInRange n | (expo n) < 0                              = Float (S.toRealFloat n :: Double)
bsonifyNumberInRange n | int64MinBound <= n && n <  int32MinBound  = Int64 $ fromIntegral (coef n) * 10 ^ (expo n)
bsonifyNumberInRange n | int32MinBound <= n && n <= int32MaxBound  = Int32 $ fromIntegral (coef n) * 10 ^ (expo n)
bsonifyNumberInRange n | int32MaxBound <  n && n <= int64MaxBound  = Int64 $ fromIntegral (coef n) * 10 ^ (expo n)
bsonifyNumberInRange _ = error "bsonifyiNumberInRange should be invoked only with n | int64MinBound < n < int64MaxBound"

