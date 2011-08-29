-- -----------------------------------------------------------------------------
-- This file is part of Skema-Common.

-- Skema-Common is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- Skema-Common is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.

-- You should have received a copy of the GNU Affero General Public License
-- along with Skema-Common.  If not, see <http://www.gnu.org/licenses/>.
-- -----------------------------------------------------------------------------
-- | Classes and functions to work with a Map indexed with a SID type
module Skema.SIDMap(
  -- Types
  SID(..), SIDMap,
  -- Functions 
  sidMapAssocs, sidMapFromList, sidMapLookup, sidMapKeys ) 
       where

-- -----------------------------------------------------------------------------
import Control.Arrow( first )
import qualified Data.IntMap as MI( IntMap, assocs, fromList, lookup, keys )

-- -----------------------------------------------------------------------------
-- | Simple Identifier or Skema Identifier, values equivalents to an Int used as
-- a 'SIDMap' key
class SID a where
  -- | Conversion to an 'Int' value.
  toInt :: a -> Int
  -- | Conversion from an 'Int' value.
  fromInt :: Int -> a
  -- | Conversion from a 'SID' instance to a different 'SID'. Default
  -- implementation uses 'fromInt' and 'toInt'
  fromSID :: SID b => b -> a
  fromSID = fromInt . toInt
  
instance SID Int where
  toInt = id
  fromInt = id

-- -----------------------------------------------------------------------------
-- | IntMap with 'SID' as key.
type SIDMap a = MI.IntMap a

-- -----------------------------------------------------------------------------
-- | Return all key/value pairs in the map in ascending key order.
sidMapAssocs :: SID k => SIDMap a -> [(k,a)]
sidMapAssocs = map (first fromInt) . MI.assocs

-- | Return all keys of the map in ascending order.
sidMapKeys :: SID k => SIDMap a -> [k]
sidMapKeys = map fromInt . MI.keys

-- | Create a map from a list of key/value pairs.
sidMapFromList :: SID k => [(k,a)] -> SIDMap a
sidMapFromList = MI.fromList . map (first toInt)

-- | Lookup the value at a key in the map.
sidMapLookup ::SID k => k -> SIDMap a -> Maybe a
sidMapLookup k = MI.lookup (toInt k)

-- -----------------------------------------------------------------------------
