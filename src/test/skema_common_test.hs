-- -----------------------------------------------------------------------------
-- This file is part of Skema.

-- Skema is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.

-- Skema is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with Skema.  If not, see <http://www.gnu.org/licenses/>.
-- -----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main( main ) where

-- -----------------------------------------------------------------------------
import Test.QuickCheck( 
  Gen, Testable, Arbitrary(..), quickCheckWithResult, elements, suchThat, 
  listOf )
import Test.QuickCheck.Test( Args(..), Result, stdArgs, isSuccess )
import qualified Test.QuickCheck.Property as P( Result, succeeded, failed )
import Text.Printf( printf )
import Control.Monad( replicateM, liftM )
import Data.Char( isHexDigit, isPrint )
import Data.List( intersect, nub )
import qualified Data.ByteString.Lazy.Char8 as LC( 
  ByteString, fromChunks, length )
import qualified Data.ByteString.Char8 as BC( ByteString, null, pack )
import qualified Data.Map as M( Map, fromList, size, keys, empty )
import qualified Data.IntMap as IM( IntMap, fromList )
import System.Exit( exitSuccess, exitFailure )
import Skema.Math( deg2rad, rad2deg )
import Skema.Types( 
  IOPointType(..), IOPointDataType(..), dataTypeVectorSize, dataTypeSize )
import Skema.Util( 
  hexByteString, byteStringHex, duplicates, topologicalSorting, fromJSONString, 
  toJSONString )
import Skema.JSON( prettyJSON )
import Skema.ProgramFlow( 
  PFNodeID, PFIOPoint(..), PFNode(..), PFKernel(..), PFArrow(..), 
  ProgramFlow(..), generateJSONString, exampleProgramFlow, unasignedOutputPoints, 
  unasignedInputPoints, decodeJSONString )
import Skema.Network()
import Skema.Concurrent()
import Skema.RunProtocol()
import Skema.DataValue()
import Skema.SIDMap( SID(..) )

-- -----------------------------------------------------------------------------
main :: IO ()
main = do
  results <- mapM (\(s,a) -> printf "%-25s: " s >> a) tests
  if (all isSuccess results) then exitSuccess else exitFailure

-- -----------------------------------------------------------------------------
printableString :: Gen String
printableString = listOf (suchThat arbitrary isPrint)

instance Arbitrary PFNodeID where
  arbitrary = fromInt `fmap` arbitrary

instance Arbitrary BC.ByteString where
  arbitrary = BC.pack `fmap` arbitrary

instance Arbitrary LC.ByteString where
  arbitrary = arbitrary >>= return . LC.fromChunks . filter (not. BC.null)

instance (Ord k, Arbitrary k,Arbitrary a) => Arbitrary (M.Map k a) where
  arbitrary = arbitrary >>= return . M.fromList
  
instance Arbitrary a => Arbitrary (IM.IntMap a) where
  arbitrary = arbitrary >>= return . IM.fromList
  
instance Arbitrary IOPointType where
  arbitrary = elements [minBound .. maxBound]
  
instance Arbitrary IOPointDataType where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary PFIOPoint where
  arbitrary = do
    d <- arbitrary
    t <- arbitrary
    return $ PFIOPoint d t
  
instance Arbitrary PFKernel where
  arbitrary = do
    body <- printableString
    extra <- printableString
    nins <- suchThat arbitrary (>0)
    nouts <- suchThat arbitrary (>0)
    ips <- replicateM nins $ do
      pname <- printableString
      d <- arbitrary
      return $ (pname,PFIOPoint d InputPoint)
    ops <- replicateM nouts $ do
      pname <- printableString
      d <- arbitrary
      return $ (pname,PFIOPoint d OutputPoint)
    return $ PFKernel body extra (M.fromList (ips ++ ops)) M.empty Nothing
  
instance Arbitrary PFNode where
  arbitrary = printableString >>= return . PFNode
    
instance Arbitrary ProgramFlow where
  arbitrary = do
    nkernels <- suchThat arbitrary (>=0)
    kernels <- liftM M.fromList $ replicateM nkernels $ do
      kname <- printableString
      kernel <- arbitrary
      return (kname,kernel)
    nodes <- if (M.size kernels == 0) 
      then return []
      else do
        nnodes <- arbitrary
        names <- replicateM nnodes (elements $ M.keys kernels)
        return $ map PFNode names
    arrows <- if (length nodes == 0)
      then return []
      else do
        narrows <- arbitrary
        replicateM narrows $ do
          aoutput <- do
            d <- suchThat arbitrary (>=(fromInt 0))
            n <- printableString
            return (d,n)
          ainput <- do
            d <- suchThat arbitrary (>=(fromInt 0))
            n <- printableString
            return (d,n)
          return $ PFArrow aoutput ainput
    return $ ProgramFlow kernels (IM.fromList (zip [1..] nodes)) arrows

-- -----------------------------------------------------------------------------
-- Skema.Math tests

prop_rad2deg :: Double -> Bool
prop_rad2deg a = abs ((rad2deg . deg2rad) a - a) < 0.001

-- -----------------------------------------------------------------------------
-- Skema.Util tests

prop_hexByteString :: String -> Bool
prop_hexByteString xs = lenbs <= (fromIntegral.length) cad
  where
    lenbs = (LC.length . hexByteString) cad 
    cad = filter isHexDigit xs

prop_hexByteString_ident :: LC.ByteString -> Bool
prop_hexByteString_ident xs = (hexByteString . byteStringHex) xs == xs

prop_duplicates :: [Int] -> Bool
prop_duplicates xs = length xs >= (length . duplicates) xs
                   
prop_topological :: [(Int,Int)] -> Bool
prop_topological xs = nodes == topo
  where
    nodes = nub topo
    topo = topologicalSorting xs
  
-- -----------------------------------------------------------------------------
-- Skema.JSON tests

prop_prettyjson_length :: ProgramFlow -> Bool
prop_prettyjson_length pf = length st <= (length . prettyJSON) st
  where st = generateJSONString pf

-- -----------------------------------------------------------------------------
-- Skema.Programflow tests

prop_jsonProgramFlow :: ProgramFlow -> Bool
prop_jsonProgramFlow pf = case decodeJSONString cad of
  Left _ -> False
  Right newpf -> newpf == pf
  where
    cad = generateJSONString pf

prop_jsonPrettyProgramFlow :: ProgramFlow -> Bool
prop_jsonPrettyProgramFlow pf = case decodeJSONString cad of
  Left _ -> False
  Right newpf -> newpf == pf
  where
    cad = prettyJSON $ generateJSONString pf

prop_encodeExample :: P.Result
prop_encodeExample = case gend of
  Just _ -> P.succeeded
  _ -> P.failed
  where
    trans = fromJSONString . toJSONString
    gend = (trans exampleProgramFlow) :: (Maybe ProgramFlow)

prop_unasignedpoints :: ProgramFlow -> Bool
prop_unasignedpoints pf = uai `intersect` uao == []
  where
    uai = unasignedInputPoints pf
    uao = unasignedOutputPoints pf

-- -----------------------------------------------------------------------------
-- Skema.Types tests

prop_read_IOPointDataType :: Int -> IOPointDataType -> Bool
prop_read_IOPointDataType d x = any (==(x,"")) (readsPrec d (showsPrec d x ""))

prop_vector_types :: IOPointDataType -> Bool
prop_vector_types x = dataTypeVectorSize x <= dataTypeSize x
  
prop_json_IOPointDataType :: IOPointDataType -> Bool
prop_json_IOPointDataType v = (fromJSONString . toJSONString) [v] == Just [v]

prop_json_IOPointType :: IOPointType -> Bool
prop_json_IOPointType v = (fromJSONString . toJSONString) [v] == Just [v]

-- -----------------------------------------------------------------------------
longCheck :: Testable prop => prop -> IO Result
longCheck = quickCheckWithResult stdArgs { maxSuccess=500 }

oneCheck :: Testable prop => prop -> IO Result
oneCheck = quickCheckWithResult stdArgs { maxSuccess=1 }

fastCheck :: Testable prop => prop -> IO Result
fastCheck = quickCheckWithResult stdArgs { maxSuccess=500, maxSize=10 }

ultraCheck :: Testable prop => prop -> IO Result
ultraCheck = quickCheckWithResult stdArgs { maxSuccess=500, maxSize=5 }

tests :: [(String, IO Result)]
tests = [
  ("Skema.Math: radians to degrees", longCheck prop_rad2deg),
  ("Skema.Types: vector sizes", longCheck prop_vector_types),
  ("Skema.Types: read IOPointDataType", longCheck prop_read_IOPointDataType),
  ("Skema.Types: json IOPointDataType", longCheck prop_json_IOPointDataType),
  ("Skema.Types: json IOPointType", longCheck prop_json_IOPointType),
  ("Skema.Util: duplicates ", longCheck prop_duplicates),
  ("Skema.Util: hex -> ByteString", longCheck prop_hexByteString),
  ("Skema.Util: hex <-> ByteString id",longCheck prop_hexByteString_ident),
  ("Skema.Util: topological sort",longCheck prop_topological),
  ("Skema.JSON: prettyJSON length", ultraCheck prop_prettyjson_length),
  ("Skema.ProgramFlow: encode Example", oneCheck prop_encodeExample),
  ("Skema.ProgramFlow: ProgramFlow -> JSON", ultraCheck prop_jsonProgramFlow),
  ("Skema.ProgramFlow: unasigned points", fastCheck prop_unasignedpoints),
  ("Skema.*: ProgramFlow -> PrettyJSON", ultraCheck prop_jsonPrettyProgramFlow)
 ]

-- -----------------------------------------------------------------------------
