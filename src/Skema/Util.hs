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
-- | General functions for Skema programs
module Skema.Util( 
  byteStringHex, hexByteString, prettyBytes, duplicates, isAcyclicGraph, 
  topologicalSorting, toJSONString, fromJSONString ) 
       where

-- -----------------------------------------------------------------------------
import Control.Arrow( (&&&) )
import Data.ByteString.Lazy( ByteString, unpack, pack )
import Data.Bits( (.&.), (.|.), shiftR, shiftL )
import Data.Char( intToDigit, digitToInt )
import Data.List( group, sort, nub )
import qualified Data.ByteString.Char8 as BSC( pack )
import qualified Data.ByteString.Lazy.Char8 as BSCL( unpack )
import Data.Aeson( FromJSON(..), ToJSON(..), encode, json )
import qualified Data.Aeson.Types as T
import Data.Attoparsec (parse, Result(..))

-- -----------------------------------------------------------------------------
-- | 'byteStringHex' converta a ByteString to a Hexadecimal representation.
byteStringHex :: ByteString -> String
byteStringHex bs = foldr paddedShowHex [] . unpack $ bs
 where
   paddedShowHex x xs = intToDigit (fromIntegral (x `shiftR` 4))
                      : intToDigit (fromIntegral (x .&. 0xf))
                      : xs

-- | 'hexByteString' converta a hexadecimal representation of a ByteString to 
-- the ByteString itself.
hexByteString :: String -> ByteString
hexByteString = pack . map (fromInteger.toInteger).groupBinary . map digitToInt
  where
    groupBinary [] = []
    groupBinary (x:[]) = [x .&. 0xf]
    groupBinary (x:y:xs) = binaryAdd x y : groupBinary xs
    binaryAdd x y = ((x .&. 0xf) `shiftL` 4) .|. (y .&. 0xf)

-- -----------------------------------------------------------------------------
prettySymbols :: [String]
prettySymbols = ["B", "KiB","MiB","GiB","TiB","PiB","EiB"]

prettyBytes :: Integral a => a -> String
prettyBytes = prettyBytes' prettySymbols . fromIntegral

prettyBytes' :: [String] -> Float -> String
prettyBytes' [] _ = error "no unit symbol"
prettyBytes' (s:[]) n = concat [show n, " ", s]
prettyBytes' (s:ss) n
  | n < 1024 = concat [show n, " ", s]
  | otherwise = prettyBytes' ss (n / 1024.0)

duplicates :: Ord a => [a] -> [a]
duplicates = map fst . filter ((>1) . snd) . map (head&&&length) . group . sort

-- -----------------------------------------------------------------------------
toJSONString :: ToJSON a => a -> String
toJSONString = BSCL.unpack . encode . toJSON

fromJSONString :: FromJSON a => String -> Maybe a
fromJSONString s = case parse json (BSC.pack s) of
  (Done _ r) -> parseMaybe' r
  _ -> Nothing

parseMaybe' :: FromJSON b => T.Value -> Maybe b
parseMaybe' r = T.parseMaybe parseJSON r

-- -----------------------------------------------------------------------------
{-
http://stackoverflow.com/questions/4168/graph-serialization/4577#4577
http://en.wikipedia.org/wiki/Topological_sorting
-}

isAcyclicGraph :: Eq a => [(a,a)] -> Bool
isAcyclicGraph edges = null graph
  where
    es = nub edges
    (graph,_) = topologicalSorting' (nodesWithoutIncoming es) es []

topologicalSorting :: Eq a => [(a,a)] -> [a]
topologicalSorting edges = reverse order
  where
    es = nub edges
    (_,order) = topologicalSorting' (nodesWithoutIncoming es) es []
topologicalSorting' :: Eq a => [a] -> [(a,a)] -> [a] -> ([(a,a)], [a])
topologicalSorting' [] gs ls = (gs,ls)
topologicalSorting' (n:xs) gs ls = topologicalSorting' (xs++newxs) newgs (n:ls)
  where
    edges = filter ((==n).fst) gs
    newgs = filter (`notElem` edges) gs
    ms = map snd edges
    newxs = filter (`notElem` (map snd newgs)) ms

graphNodes :: Eq a => [(a,a)] -> [a]
graphNodes = nub . uncurry (++) . unzip

nodesWithoutIncoming :: Eq a => [(a,a)] -> [a]
nodesWithoutIncoming edges = filter notInput $ graphNodes edges
  where
    notInput x = x `notElem` (map snd edges)

-- -----------------------------------------------------------------------------
