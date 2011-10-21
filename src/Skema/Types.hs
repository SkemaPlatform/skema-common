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
-- | Useful types for Skema programs
module Skema.Types( 
  IOPointType(..), IOPointDataType(..), openclTypeNames, isSameBaseType,
  dataTypeSize, dataTypeBase, dataTypeVectorSize ) 
       where

-- -----------------------------------------------------------------------------
import Control.Applicative( pure )
import Control.Monad( mzero )
import qualified Data.Map as M( Map, (!), fromList, lookup )
import Data.Tuple( swap )
import Data.Aeson( Value(..), ToJSON(..), FromJSON(..) )
import qualified Data.Text as Txt( unpack )

-- -----------------------------------------------------------------------------
-- | Type of a comunication point of a node
data IOPointType = InputPoint -- ^ Input
                 | OutputPoint -- ^ Output
                   deriving( Show, Read, Eq, Enum, Bounded )

-- -----------------------------------------------------------------------------
-- | Built-in data types of OpenCL programs
data IOPointDataType = IOchar | IOuchar | IOshort | IOushort
                     | IOint | IOuint | IOlong | IOulong
                     | IOfloat
                     | IOchar2 | IOuchar2 | IOshort2 | IOushort2
                     | IOint2 | IOuint2 | IOlong2 | IOulong2
                     | IOfloat2
                     | IOchar4 | IOuchar4 | IOshort4 | IOushort4
                     | IOint4 | IOuint4 | IOlong4 | IOulong4
                     | IOfloat4
                     | IOchar8 | IOuchar8 | IOshort8 | IOushort8
                     | IOint8 | IOuint8 | IOlong8 | IOulong8
                     | IOfloat8
                     | IOchar16 | IOuchar16 | IOshort16 | IOushort16
                     | IOint16 | IOuint16 | IOlong16 | IOulong16
                     | IOfloat16
                     deriving( Eq, Enum, Ord, Bounded )

dataTypeBases :: [(IOPointDataType,IOPointDataType)]
dataTypeBases = [
  (IOchar,IOchar), (IOuchar,IOuchar), (IOshort,IOshort), (IOushort,IOushort), 
  (IOint,IOint), (IOuint,IOuint), (IOlong,IOlong), (IOulong,IOulong), 
  (IOfloat,IOfloat), (IOchar2,IOchar), (IOuchar2,IOuchar), (IOshort2,IOshort), 
  (IOushort2,IOushort), (IOint2,IOint), (IOuint2,IOuint), (IOlong2,IOlong), 
  (IOulong2,IOulong), (IOfloat2,IOfloat), (IOchar4,IOchar), (IOuchar4,IOuchar), 
  (IOshort4,IOshort), (IOushort4,IOushort), (IOint4,IOint), (IOuint4,IOuint), 
  (IOlong4,IOlong), (IOulong4,IOulong), (IOfloat4,IOfloat), (IOchar8,IOchar),
  (IOuchar8,IOuchar), (IOshort8,IOshort), (IOushort8,IOushort), 
  (IOint8,IOint), (IOuint8,IOuint), (IOlong8,IOlong), (IOulong8,IOulong), 
  (IOfloat8,IOfloat), (IOchar16,IOchar), (IOuchar16,IOuchar), 
  (IOshort16,IOshort), (IOushort16,IOushort), (IOint16,IOint), 
  (IOuint16,IOuint), (IOlong16,IOlong), (IOulong16,IOulong), 
  (IOfloat16,IOfloat)]

-- | get the equivalent scalar Data Type of a OpenCL Data Type. With integral
-- types always return the unsigned one. Example: int16 base type is uint.
dataTypeBase :: IOPointDataType -> IOPointDataType
dataTypeBase = maybe (error "no datatype base") id . flip lookup dataTypeBases

-- | Check if two data types have the same scalar type.
isSameBaseType :: IOPointDataType -> IOPointDataType -> Bool
isSameBaseType a b = a == b || (dataTypeBase a) == (dataTypeBase b)

dataTypeNames :: [(IOPointDataType,String)]
dataTypeNames = [
  (IOchar, "char"), (IOuchar, "uchar"), (IOshort, "short"),
  (IOushort, "ushort"), (IOint, "int"), (IOuint, "uint"), (IOlong, "long"),
  (IOulong, "ulong"), (IOfloat, "float"), (IOchar2, "char2"), 
  (IOuchar2, "uchar2"), (IOshort2, "short2"), (IOushort2, "ushort2"),
  (IOint2, "int2"), (IOuint2, "uint2"), (IOlong2, "long2"), 
  (IOulong2, "ulong2"), (IOfloat2, "float2"), (IOchar4, "char4"),
  (IOuchar4, "uchar4"), (IOshort4, "short4"), (IOushort4, "ushort4"),
  (IOint4, "int4"), (IOuint4, "uint4"), (IOlong4, "long4"), 
  (IOulong4, "ulong4"), (IOfloat4, "float4"), (IOchar8, "char8"),
  (IOuchar8, "uchar8"), (IOshort8, "short8"), (IOushort8, "ushort8"),
  (IOint8, "int8"), (IOuint8, "uint8"), (IOlong8, "long8"), 
  (IOulong8, "ulong8"), (IOfloat8, "float8"), (IOchar16, "char16"),
  (IOuchar16, "uchar16"), (IOshort16, "short16"), (IOushort16, "ushort16"),
  (IOint16, "int16"), (IOuint16, "uint16"), (IOlong16, "long16"),
  (IOulong16, "ulong16"), (IOfloat16, "float16")]

dataTypeShowTable :: M.Map IOPointDataType String
dataTypeShowTable = M.fromList $ dataTypeNames

dataTypeReadTable :: M.Map String IOPointDataType
dataTypeReadTable = M.fromList . map swap $ dataTypeNames

-- | names of the OpenCL Scalar and Vector Data Types.
openclTypeNames :: [String]
openclTypeNames = map snd dataTypeNames

dataTypeSizes :: [(IOPointDataType,Int)]
dataTypeSizes = [
  (IOchar,1), (IOuchar,1), (IOshort,2), (IOushort,2), (IOint,4), (IOuint,4), 
  (IOlong,8), (IOulong,8), (IOfloat,4), (IOchar2,2), (IOuchar2,2), (IOshort2,4),
  (IOushort2,4), (IOint2,8), (IOuint2,8), (IOlong2,16), (IOulong2,16), 
  (IOfloat2,8), (IOchar4,4), (IOuchar4,4), (IOshort4,8), (IOushort4,8), 
  (IOint4,16), (IOuint4,16), (IOlong4,32), (IOulong4,32), (IOfloat4,16), 
  (IOchar8,8), (IOuchar8,8), (IOshort8,16), (IOushort8,16), (IOint8,32), 
  (IOuint8,32), (IOlong8,64), (IOulong8,64), (IOfloat8,32), (IOchar16,16), 
  (IOuchar16,16), (IOshort16,32), (IOushort16,32), (IOint16,64), (IOuint16,64), 
  (IOlong16,128), (IOulong16,128), (IOfloat16,64)]

-- | get the size in bytes of a OpenCL Data Type.
dataTypeSize :: IOPointDataType -> Int
dataTypeSize = maybe (error "no datatype size") id . flip lookup dataTypeSizes

dataTypeVectorSizes :: [(IOPointDataType,Int)]
dataTypeVectorSizes = [
  (IOchar,1), (IOuchar,1), (IOshort,1), (IOushort,1), (IOint,1), (IOuint,1), 
  (IOlong,1), (IOulong,1), (IOfloat,1), (IOchar2,2), (IOuchar2,2), (IOshort2,2),
  (IOushort2,2), (IOint2,2), (IOuint2,2), (IOlong2,2), (IOulong2,2), 
  (IOfloat2,2), (IOchar4,4), (IOuchar4,4), (IOshort4,4), (IOushort4,4), 
  (IOint4,4), (IOuint4,4), (IOlong4,4), (IOulong4,4), (IOfloat4,4), 
  (IOchar8,8), (IOuchar8,8), (IOshort8,8), (IOushort8,8), (IOint8,8), 
  (IOuint8,8), (IOlong8,8), (IOulong8,8), (IOfloat8,8), (IOchar16,16), 
  (IOuchar16,16), (IOshort16,16), (IOushort16,16), (IOint16,16), (IOuint16,16), 
  (IOlong16,16), (IOulong16,16), (IOfloat16,16)]

-- | get the number of elements of the vector of a OpenCL Data Type.
dataTypeVectorSize :: IOPointDataType -> Int
dataTypeVectorSize = maybe (error "no datatype vector size") id 
                     . flip lookup dataTypeVectorSizes

-- -----------------------------------------------------------------------------
instance Show IOPointDataType where
  show v = dataTypeShowTable M.! v

instance Read IOPointDataType where
  readsPrec _ r = do
    (k,t) <- lex r
    maybe [] (\v -> [(v,t)]) $ M.lookup k dataTypeReadTable

-- -----------------------------------------------------------------------------
instance ToJSON IOPointType where
  toJSON = toJSON . show
  
instance FromJSON IOPointType where
  parseJSON (String v) = case (reads . Txt.unpack $ v) of
      [] -> mzero
      (iot,_):_ -> pure iot
  parseJSON _ = mzero
  
instance ToJSON IOPointDataType where
  toJSON = toJSON . show
  
instance FromJSON IOPointDataType where
  parseJSON (String v) = case (reads . Txt.unpack $ v) of
      [] -> mzero
      (iot,_):_ -> pure iot
  parseJSON _ = mzero
  
-- -----------------------------------------------------------------------------
