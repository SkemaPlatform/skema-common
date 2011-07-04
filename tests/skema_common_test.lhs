%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of Skema.

% Skema is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.

% Skema is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.

% You should have received a copy of the GNU General Public License
% along with Skema.  If not, see <http://www.gnu.org/licenses/>.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main( main ) where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Test.QuickCheck
  ( Testable, Arbitrary(..), Args(..), stdArgs, quickCheckWith, elements, 
    suchThat )
import Test.QuickCheck.Property( Result, succeeded, failed )
import Text.Printf( printf )
import qualified Text.JSON as JSON( encode, decode, Result(..) )
import Control.Monad( replicateM )
import Data.Char( isHexDigit )
import Data.List( intersect )
import qualified Data.ByteString.Lazy.Char8 as LC
  ( ByteString, fromChunks, length )
import qualified Data.ByteString.Char8 as BC( ByteString, null, pack )
import qualified Data.Map as M( Map, fromList, size, keys )
import qualified Data.IntMap as IM( IntMap, fromList )
import Skema.Math( deg2rad, rad2deg )
import Skema.Types( IOPointType(..), IOPointDataType(..) )
import Skema.Util( hexByteString, byteStringHex, __ )
import Skema.JSON( prettyJSON )
import Skema.ProgramFlow
  ( PFIOPoint(..), PFNode(..), PFKernel(..), PFArrow(..), ProgramFlow(..), 
    generateJSONString, exampleProgramFlow, unasignedOutputPoints, 
    unasignedInputPoints, decodeJSONString )
import Skema.Network()
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
main :: IO ()
main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
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
    name <- arbitrary
    nins <- suchThat arbitrary (>0)
    nouts <- suchThat arbitrary (>0)
    ips <- replicateM nins $ do
      pname <- arbitrary
      d <- arbitrary
      return $ (pname,PFIOPoint d InputPoint)
    ops <- replicateM nouts $ do
      pname <- arbitrary
      d <- arbitrary
      return $ (pname,PFIOPoint d OutputPoint)
    return $ PFKernel name (M.fromList (ips ++ ops))
  
instance Arbitrary PFNode where
  arbitrary = arbitrary >>= return . PFNode
    
instance Arbitrary ProgramFlow where
  arbitrary = do
    kernels <- arbitrary
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
          aoutput <- arbitrary
          ainput <- arbitrary
          return $ PFArrow aoutput ainput
    return $ ProgramFlow kernels (IM.fromList (zip [1..] nodes)) arrows
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Skema.Math tests

\begin{code}
prop_rad2deg :: Double -> Bool
prop_rad2deg a = abs ((rad2deg . deg2rad) a - a) < 0.001
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Skema.Util tests

\begin{code}
prop_hexByteString :: String -> Bool
prop_hexByteString xs = lenbs <= (fromIntegral.length) cad
  where
    lenbs = (LC.length . hexByteString) cad 
    cad = filter isHexDigit xs
\end{code}

\begin{code}
prop_hexByteString_ident :: LC.ByteString -> Bool
prop_hexByteString_ident xs = (hexByteString . byteStringHex) xs == xs
\end{code}

\begin{code}
prop_i18n :: String -> Bool
prop_i18n xs = '\NUL' `elem` xs || __ xs == xs
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Skema.JSON tests

\begin{code}
prop_prettyjson_length :: ProgramFlow -> Bool
prop_prettyjson_length pf = length st <= (length . prettyJSON) st
  where st = JSON.encode pf
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Skema.Programflow tests

\begin{code}
prop_jsonProgramFlow :: ProgramFlow -> Bool
prop_jsonProgramFlow pf = case decodeJSONString cad of
  Left _ -> False
  Right newpf -> newpf == pf
  where
    cad = generateJSONString pf
\end{code}

\begin{code}
prop_encodeExample :: Result
prop_encodeExample = case gend of
  JSON.Ok _ -> succeeded
  _ -> failed
  where
    trans = JSON.decode . JSON.encode
    gend = (trans exampleProgramFlow) :: (JSON.Result ProgramFlow)
\end{code}

\begin{code}
prop_unasignedpoints :: ProgramFlow -> Bool
prop_unasignedpoints pf = uai `intersect` uao == []
  where
    uai = unasignedInputPoints pf
    uao = unasignedOutputPoints pf
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Skema.Types tests

\begin{code}
prop_read_IOPointDataType :: Int -> IOPointDataType -> Bool
prop_read_IOPointDataType d x = any (==(x,"")) (readsPrec d (showsPrec d x ""))
\end{code}

\begin{code}
prop_json_IOPointDataType :: IOPointDataType -> Bool
prop_json_IOPointDataType v = (JSON.decode . JSON.encode) v == JSON.Ok v
\end{code}

\begin{code}
prop_json_IOPointType :: IOPointType -> Bool
prop_json_IOPointType v = (JSON.decode . JSON.encode) v == JSON.Ok v
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
longCheck :: Testable prop => prop -> IO ()
longCheck = quickCheckWith stdArgs { maxSuccess=500 }
\end{code}

\begin{code}
oneCheck :: Testable prop => prop -> IO ()
oneCheck = quickCheckWith stdArgs { maxSuccess=1 }
\end{code}

\begin{code}
fastCheck :: Testable prop => prop -> IO ()
fastCheck = quickCheckWith stdArgs { maxSuccess=500, maxSize=10 }
\end{code}

\begin{code}
tests :: [(String, IO ())]
tests = [
  ("Skema.Math: radians to degrees", longCheck prop_rad2deg),
  ("Skema.Types: read IOPointDataType", longCheck prop_read_IOPointDataType),
  ("Skema.Types: json IOPointDataType", longCheck prop_json_IOPointDataType),
  ("Skema.Types: json IOPointType", longCheck prop_json_IOPointType),
  ("Skema.Util: hex -> ByteString", longCheck prop_hexByteString),
  ("Skema.Util: hex <-> ByteString id",longCheck prop_hexByteString_ident),
  ("Skema.Util: i18n",longCheck prop_i18n),
  ("Skema.JSON: prettyJSON length", fastCheck prop_prettyjson_length),
  ("Skema.ProgramFlow: encode Example", oneCheck prop_encodeExample),
  ("Skema.ProgramFlow: ProgramFlow -> JSON", fastCheck prop_jsonProgramFlow),
  ("Skema.ProgramFlow: unasigned points", fastCheck prop_unasignedpoints) 
 ]
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
