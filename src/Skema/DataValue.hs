-- -----------------------------------------------------------------------------
-- This file is part of Skema-Common.

-- Skema-Common is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.

-- Skema-Common is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with Skema-Common.  If not, see <http://www.gnu.org/licenses/>.
-- -----------------------------------------------------------------------------
-- | Data Values are haskell types representing Skema Program Types values
module Skema.DataValue( 
  DataValue(..), updateDataValue, extractValue, valueToByteString, 
  valuesToByteString, convertToDataValues, convertNValues, dvToIntegral, 
  dvToFloat )
       where

-- -----------------------------------------------------------------------------
import Data.Word( Word8, Word16, Word32, Word64 )
import Data.Int( Int8, Int16, Int32, Int64 )
import Data.Bits( (.&.), (.|.), shiftR, shiftL )
import Data.Binary.IEEE754( 
  floatToWord, wordToFloat, doubleToWord, wordToDouble )
import GHC.Float( double2Float, float2Double )
import qualified Data.ByteString as B( 
  ByteString, empty, pack, index, singleton, concat, drop, take )
import Skema.Types( 
  IOPointDataType(..), dataTypeBase, dataTypeVectorSize, dataTypeSize )

-- -----------------------------------------------------------------------------
-- | Value with a corresponding OpenCL Type
data DataValue = DVchar Int8 | DVuchar Word8 | DVshort Int16 
               | DVushort Word16 | DVint Int32 | DVuint Word32 
               | DVlong Int64 | DVulong Word64 | DVfloat Float
               | DVdouble Double
               deriving( Show )
                       
-- -----------------------------------------------------------------------------
-- | put a real value into a `DataValue`, converting to the same type.
updateDataValue :: Double -- ^ New value.
                   -> DataValue -- ^ Type to convert to.
                   -> DataValue
updateDataValue d (DVchar _) = DVchar $ round d
updateDataValue d (DVuchar _) = DVuchar $ round d
updateDataValue d (DVshort _) = DVshort $ round d
updateDataValue d (DVushort _) = DVushort $ round d
updateDataValue d (DVint _) = DVint $ round d
updateDataValue d (DVuint _) = DVuint $ round d
updateDataValue d (DVlong _) = DVlong $ round d
updateDataValue d (DVulong _) = DVulong $ round d
updateDataValue d (DVfloat _) = DVfloat $ double2Float d
updateDataValue d (DVdouble _) = DVdouble d

-- -----------------------------------------------------------------------------
-- | get a real value from a `DataValue`, converting it to `Double`.
extractValue :: DataValue -> Double
extractValue (DVchar v) = fromIntegral v
extractValue (DVuchar v) = fromIntegral v
extractValue (DVshort v) = fromIntegral v
extractValue (DVushort v) = fromIntegral v
extractValue (DVint v) = fromIntegral v
extractValue (DVuint v) = fromIntegral v
extractValue (DVlong v) = fromIntegral v
extractValue (DVulong v) = fromIntegral v
extractValue (DVfloat v) = float2Double v
extractValue (DVdouble v) = v

-- -----------------------------------------------------------------------------
-- | Numerical Type convertible to a ByteString, in Big Endian or Little Endian.
class Num a => ToByteString a where
  toByteString_le, toByteString_be :: a -> B.ByteString
  toByteString_le = const B.empty
  toByteString_be = const B.empty
  fromByteString_le, fromByteString_be :: B.ByteString -> a
  fromByteString_le = const 0
  fromByteString_be = const 0
  
instance ToByteString Int8 where
  toByteString_le = B.singleton . fromIntegral
  toByteString_be = toByteString_le
  fromByteString_le = fromIntegral . (`B.index` 0)
  fromByteString_be = fromByteString_le

instance ToByteString Word8 where
  toByteString_le = B.singleton
  toByteString_be = toByteString_le
  fromByteString_le = (`B.index` 0)
  fromByteString_be = fromByteString_le

instance ToByteString Int16 where
  toByteString_le w = B.pack [a,b]
    where
      a = fromIntegral $ (w .&. 0xff)
      b = fromIntegral $ (w .&. 0xff00) `shiftR` 8
  toByteString_be w = B.pack [b,a]
    where
      a = fromIntegral $ (w .&. 0xff)
      b = fromIntegral $ (w .&. 0xff00) `shiftR` 8
  fromByteString_le w = a .|. b
    where
      a = fromIntegral $ w `B.index` 0
      b = (fromIntegral $ w `B.index` 1) `shiftL` 8
  fromByteString_be w = a .|. b
    where
      a = fromIntegral $ w `B.index` 1
      b = (fromIntegral $ w `B.index` 0) `shiftL` 8

instance ToByteString Word16 where
  toByteString_le w = B.pack [a,b]
    where
      a = fromIntegral $ (w .&. 0xff)
      b = fromIntegral $ (w .&. 0xff00) `shiftR` 8
  toByteString_be w = B.pack [b,a]
    where
      a = fromIntegral $ (w .&. 0xff)
      b = fromIntegral $ (w .&. 0xff00) `shiftR` 8
  fromByteString_le w = a .|. b
    where
      a = fromIntegral $ w `B.index` 0
      b = (fromIntegral $ w `B.index` 1) `shiftL` 8
  fromByteString_be w = a .|. b
    where
      a = fromIntegral $ w `B.index` 1
      b = (fromIntegral $ w `B.index` 0) `shiftL` 8

instance ToByteString Int32 where
  toByteString_le w = B.pack [a,b,c,d]
    where
      a = fromIntegral $ (w .&. 0xff)
      b = fromIntegral $ (w .&. 0xff00) `shiftR` 8
      c = fromIntegral $ (w .&. 0xff0000) `shiftR` 16
      d = fromIntegral $ (w .&. 0xff000000) `shiftR` 24
  toByteString_be w = B.pack [d,c,b,a]
    where
      a = fromIntegral $ (w .&. 0xff)
      b = fromIntegral $ (w .&. 0xff00) `shiftR` 8
      c = fromIntegral $ (w .&. 0xff0000) `shiftR` 16
      d = fromIntegral $ (w .&. 0xff000000) `shiftR` 24
  fromByteString_le w = a .|. b .|. c .|. d
    where
      a = fromIntegral $ w `B.index` 0
      b = (fromIntegral $ w `B.index` 1) `shiftL` 8
      c = (fromIntegral $ w `B.index` 2) `shiftL` 16
      d = (fromIntegral $ w `B.index` 3) `shiftL` 24
  fromByteString_be w = a .|. b .|. c .|. d
    where
      a = fromIntegral $ w `B.index` 3
      b = (fromIntegral $ w `B.index` 2) `shiftL` 8
      c = (fromIntegral $ w `B.index` 1) `shiftL` 16
      d = (fromIntegral $ w `B.index` 0) `shiftL` 24

instance ToByteString Word32 where
  toByteString_le w = B.pack [a,b,c,d]
    where
      a = fromIntegral $ (w .&. 0xff)
      b = fromIntegral $ (w .&. 0xff00) `shiftR` 8
      c = fromIntegral $ (w .&. 0xff0000) `shiftR` 16
      d = fromIntegral $ (w .&. 0xff000000) `shiftR` 24
  toByteString_be w = B.pack [d,c,b,a]
    where
      a = fromIntegral $ (w .&. 0xff)
      b = fromIntegral $ (w .&. 0xff00) `shiftR` 8
      c = fromIntegral $ (w .&. 0xff0000) `shiftR` 16
      d = fromIntegral $ (w .&. 0xff000000) `shiftR` 24
  fromByteString_le w = a .|. b .|. c .|. d
    where
      a = fromIntegral $ w `B.index` 0
      b = (fromIntegral $ w `B.index` 1) `shiftL` 8
      c = (fromIntegral $ w `B.index` 2) `shiftL` 16
      d = (fromIntegral $ w `B.index` 3) `shiftL` 24
  fromByteString_be w = a .|. b .|. c .|. d
    where
      a = fromIntegral $ w `B.index` 3
      b = (fromIntegral $ w `B.index` 2) `shiftL` 8
      c = (fromIntegral $ w `B.index` 1) `shiftL` 16
      d = (fromIntegral $ w `B.index` 0) `shiftL` 24

instance ToByteString Word64 where
  toByteString_le w = B.pack [a,b,c,d,e,f,g,h]
    where
      a = fromIntegral $ (w .&. 0xff)
      b = fromIntegral $ (w .&. 0xff00) `shiftR` 8
      c = fromIntegral $ (w .&. 0xff0000) `shiftR` 16
      d = fromIntegral $ (w .&. 0xff000000) `shiftR` 24
      e = fromIntegral $ (w .&. 0xff00000000) `shiftR` 32
      f = fromIntegral $ (w .&. 0xff0000000000) `shiftR` 40
      g = fromIntegral $ (w .&. 0xff000000000000) `shiftR` 48
      h = fromIntegral $ (w .&. 0xff00000000000000) `shiftR` 56
  toByteString_be w = B.pack [h,g,f,e,d,c,b,a]
    where
      a = fromIntegral $ (w .&. 0xff)
      b = fromIntegral $ (w .&. 0xff00) `shiftR` 8
      c = fromIntegral $ (w .&. 0xff0000) `shiftR` 16
      d = fromIntegral $ (w .&. 0xff000000) `shiftR` 24
      e = fromIntegral $ (w .&. 0xff00000000) `shiftR` 32
      f = fromIntegral $ (w .&. 0xff0000000000) `shiftR` 40
      g = fromIntegral $ (w .&. 0xff000000000000) `shiftR` 48
      h = fromIntegral $ (w .&. 0xff00000000000000) `shiftR` 56
  fromByteString_le w = a .|. b .|. c .|. d .|. e .|. f .|. g .|. h
    where
      a = fromIntegral $ w `B.index` 0
      b = (fromIntegral $ w `B.index` 1) `shiftL` 8
      c = (fromIntegral $ w `B.index` 2) `shiftL` 16
      d = (fromIntegral $ w `B.index` 3) `shiftL` 24
      e = (fromIntegral $ w `B.index` 4) `shiftL` 32
      f = (fromIntegral $ w `B.index` 5) `shiftL` 40
      g = (fromIntegral $ w `B.index` 6) `shiftL` 48
      h = (fromIntegral $ w `B.index` 7) `shiftL` 56
  fromByteString_be w = a .|. b .|. c .|. d .|. e .|. f .|. g .|. h
    where
      a = fromIntegral $ w `B.index` 7
      b = (fromIntegral $ w `B.index` 6) `shiftL` 8
      c = (fromIntegral $ w `B.index` 5) `shiftL` 16
      d = (fromIntegral $ w `B.index` 4) `shiftL` 24
      e = (fromIntegral $ w `B.index` 3) `shiftL` 32
      f = (fromIntegral $ w `B.index` 2) `shiftL` 40
      g = (fromIntegral $ w `B.index` 1) `shiftL` 48
      h = (fromIntegral $ w `B.index` 0) `shiftL` 56

instance ToByteString Int64 where
  toByteString_le w = B.pack [a,b,c,d,e,f,g,h]
    where
      a = fromIntegral $ (w .&. 0xff)
      b = fromIntegral $ (w .&. 0xff00) `shiftR` 8
      c = fromIntegral $ (w .&. 0xff0000) `shiftR` 16
      d = fromIntegral $ (w .&. 0xff000000) `shiftR` 24
      e = fromIntegral $ (w .&. 0xff00000000) `shiftR` 32
      f = fromIntegral $ (w .&. 0xff0000000000) `shiftR` 40
      g = fromIntegral $ (w .&. 0xff000000000000) `shiftR` 48
      h = fromIntegral $ (w .&. 0xff00000000000000) `shiftR` 56
  toByteString_be w = B.pack [h,g,f,e,d,c,b,a]
    where
      a = fromIntegral $ (w .&. 0xff)
      b = fromIntegral $ (w .&. 0xff00) `shiftR` 8
      c = fromIntegral $ (w .&. 0xff0000) `shiftR` 16
      d = fromIntegral $ (w .&. 0xff000000) `shiftR` 24
      e = fromIntegral $ (w .&. 0xff00000000) `shiftR` 32
      f = fromIntegral $ (w .&. 0xff0000000000) `shiftR` 40
      g = fromIntegral $ (w .&. 0xff000000000000) `shiftR` 48
      h = fromIntegral $ (w .&. 0xff00000000000000) `shiftR` 56
  fromByteString_le w = a .|. b .|. c .|. d .|. e .|. f .|. g .|. h
    where
      a = fromIntegral $ w `B.index` 0
      b = (fromIntegral $ w `B.index` 1) `shiftL` 8
      c = (fromIntegral $ w `B.index` 2) `shiftL` 16
      d = (fromIntegral $ w `B.index` 3) `shiftL` 24
      e = (fromIntegral $ w `B.index` 4) `shiftL` 32
      f = (fromIntegral $ w `B.index` 5) `shiftL` 40
      g = (fromIntegral $ w `B.index` 6) `shiftL` 48
      h = (fromIntegral $ w `B.index` 7) `shiftL` 56
  fromByteString_be w = a .|. b .|. c .|. d .|. e .|. f .|. g .|. h
    where
      a = fromIntegral $ w `B.index` 7
      b = (fromIntegral $ w `B.index` 6) `shiftL` 8
      c = (fromIntegral $ w `B.index` 5) `shiftL` 16
      d = (fromIntegral $ w `B.index` 4) `shiftL` 24
      e = (fromIntegral $ w `B.index` 3) `shiftL` 32
      f = (fromIntegral $ w `B.index` 2) `shiftL` 40
      g = (fromIntegral $ w `B.index` 1) `shiftL` 48
      h = (fromIntegral $ w `B.index` 0) `shiftL` 56

instance ToByteString Float where
  toByteString_le = toByteString_le . floatToWord
  toByteString_be = toByteString_be . floatToWord
  fromByteString_le = wordToFloat . fromByteString_le
  fromByteString_be = wordToFloat . fromByteString_be
      
instance ToByteString Double where
  toByteString_le = toByteString_le . doubleToWord
  toByteString_be = toByteString_be . doubleToWord
  fromByteString_le = wordToDouble . fromByteString_le
  fromByteString_be = wordToDouble . fromByteString_be
  
-- -----------------------------------------------------------------------------
-- | Pack a `DataValue` into a ByteString.
valueToByteString :: DataValue -> B.ByteString
valueToByteString (DVchar v) = toByteString_le v
valueToByteString (DVuchar v) = toByteString_le v
valueToByteString (DVshort v) = toByteString_le v
valueToByteString (DVushort v) = toByteString_le v
valueToByteString (DVint v) = toByteString_le v
valueToByteString (DVuint v) = toByteString_le v
valueToByteString (DVlong v) = toByteString_le v
valueToByteString (DVulong v) = toByteString_le v
valueToByteString (DVfloat v) = toByteString_le v
valueToByteString (DVdouble v) = toByteString_le v
  
-- -----------------------------------------------------------------------------
-- | Pack a `DataValue` list into a ByteString.
valuesToByteString :: [DataValue] -> B.ByteString
valuesToByteString = B.concat . map valueToByteString

-- -----------------------------------------------------------------------------
-- | Extract from a ByteString the `DataValue`.
convertToDataValues :: B.ByteString 
                       -> IOPointDataType -- ^ Type to extract.
                       -> [DataValue]
convertToDataValues b IOchar = [DVchar $ fromByteString_le b]
convertToDataValues b IOuchar = [DVuchar $ fromByteString_le b]
convertToDataValues b IOshort = [DVshort $ fromByteString_le b]
convertToDataValues b IOushort = [DVushort $ fromByteString_le b]
convertToDataValues b IOint = [DVint $ fromByteString_le b]
convertToDataValues b IOuint = [DVuint $ fromByteString_le b]
convertToDataValues b IOlong = [DVlong $ fromByteString_le b]
convertToDataValues b IOulong = [DVulong $ fromByteString_le b]
convertToDataValues b IOfloat = [DVfloat $ fromByteString_le b]
convertToDataValues b IOdouble = [DVdouble $ fromByteString_le b]
convertToDataValues b t = convertNValues b (dataTypeBase t) (dataTypeVectorSize t)
  
-- | Extract n `DataValue` elements from a ByteString.
convertNValues :: B.ByteString 
                  -> IOPointDataType -- ^ Type to extract.
                  -> Int -- ^ Number of elements to extract.
                  -> [DataValue]
convertNValues b t n = concatMap (\x-> convertToDataValues x t)
                       $ map (B.take (dataTypeSize t)) 
                       $ take n $ iterate (B.drop (dataTypeSize t)) b

-- -----------------------------------------------------------------------------
-- | Convert `DataValue` to Integral type.
dvToIntegral :: Integral a => DataValue -> a
dvToIntegral (DVchar v) = fromIntegral v
dvToIntegral (DVuchar v) = fromIntegral v
dvToIntegral (DVshort v) = fromIntegral v
dvToIntegral (DVushort v) = fromIntegral v
dvToIntegral (DVint v) =  fromIntegral v
dvToIntegral (DVuint v) =  fromIntegral v
dvToIntegral (DVlong v) =  fromIntegral v
dvToIntegral (DVulong v) = fromIntegral v
dvToIntegral (DVfloat v) = round v
dvToIntegral (DVdouble v) = round v

-- | Conversion to `Float`.
dvToFloat :: DataValue -> Float
dvToFloat (DVchar v) = fromIntegral v
dvToFloat (DVuchar v) = fromIntegral v
dvToFloat (DVshort v) = fromIntegral v
dvToFloat (DVushort v) = fromIntegral v
dvToFloat (DVint v) =  fromIntegral v
dvToFloat (DVuint v) =  fromIntegral v
dvToFloat (DVlong v) =  fromIntegral v
dvToFloat (DVulong v) = fromIntegral v
dvToFloat (DVfloat v) = v
dvToFloat (DVdouble v) = double2Float v

-- -----------------------------------------------------------------------------
