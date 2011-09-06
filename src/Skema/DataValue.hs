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
module Skema.DataValue( DataValue(..) )
       where

-- -----------------------------------------------------------------------------
import Data.Word( Word8, Word16, Word32, Word64 )
import Data.Int( Int8, Int16, Int32, Int64 )
import Data.Bits( (.&.), (.|.), shiftR, shiftL )
import qualified Data.ByteString as B( 
  ByteString, pack, index, singleton )

-- -----------------------------------------------------------------------------
data DataValue = DVchar Int8 | DVuchar Word8 | DVshort Int16 
               | DVushort Word16 | DVint Int32 | DVuint Word32 
               | DVlong Int64 | DVulong Word64 | DVfloat Float
               deriving( Show )
                       
-- -----------------------------------------------------------------------------
class ToByteString a where
  toByteString_le :: a -> B.ByteString
  toByteString_be :: a -> B.ByteString
  fromByteString_le :: B.ByteString -> a
  fromByteString_be :: B.ByteString -> a
  
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

-- -----------------------------------------------------------------------------
