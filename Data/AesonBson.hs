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
import qualified Data.Attoparsec.Number as Atto
import           Data.Int
import           Data.Monoid
import qualified Data.HashMap.Strict as HashMap (fromList, toList)
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vector (fromList, toList)

data OutOfBoundNumber = OutOfBoundMin
                      | OutOfBoundMax

-- | Converts an AESON object to a BSON document. Will yeld an error for JSON numbers that are too big.
bsonifyError :: AESON.Object -> BSON.Document
bsonifyError = bsonify errorRange

-- | Converts an AESON object to a BSON document. Will bound JSON numbers that are too big.
bsonifyBound :: AESON.Object -> BSON.Document
bsonifyBound = bsonify bound

-- | Converts an AESON object to a BSON document. The user can provide a function to deal with JSON numbers that are too big.
bsonify :: (OutOfBoundNumber -> BSON.Value) -> AESON.Object -> BSON.Document
bsonify f o = map (\(t, v) -> t := bsonifyValue f v) $ HashMap.toList o

-- | Converts a BSON document to an AESON object.
aesonify :: BSON.Document -> AESON.Object
aesonify = HashMap.fromList . map (\(l := v) -> (l, aesonifyValue v))


-- | Helpers

-- | Converts a JSON value to BSON.
bsonifyValue :: (OutOfBoundNumber -> BSON.Value) -> AESON.Value -> BSON.Value
bsonifyValue f (Object obj)        = Doc $ bsonify f obj
bsonifyValue f (AESON.Array array) = BSON.Array . map (bsonifyValue f) . Vector.toList $ array
bsonifyValue _ (AESON.String str)  = BSON.String str
bsonifyValue _ (AESON.Bool b)      = BSON.Bool b
bsonifyValue _ (AESON.Null)        = BSON.Null
bsonifyValue f (AESON.Number n)    
   | exponent < 0                              = Float (S.toRealFloat n :: Double)
   | n < int64MinBound                         = f OutOfBoundMin
   | int64MinBound <= n && n <  int32MinBound  = Int64 $ fromIntegral coefficient * 10 ^ exponent
   | int32MinBound <= n && n <= int32MaxBound  = Int32 $ fromIntegral coefficient * 10 ^ exponent
   | int32MaxBound <  n && n <= int64MaxBound  = Int64 $ fromIntegral coefficient * 10 ^ exponent
   | n > int64MaxBound                         = f OutOfBoundMax
     where
       exponent       = S.base10Exponent n
       coefficient    = S.coefficient n

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

errorRange :: OutOfBoundNumber -> BSON.Value
errorRange OutOfBoundMin = error $ "Integer out of min range"
errorRange OutOfBoundMax = error $ "Integer out of max range"

bound :: OutOfBoundNumber -> BSON.Value
bound OutOfBoundMin = Int64 minBound
bound OutOfBoundMax = Int64 maxBound

