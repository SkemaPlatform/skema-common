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
import Text.Printf( printf )
import Control.Monad( replicateM )
import Data.Char( isHexDigit )
import qualified Data.ByteString.Lazy.Char8 as LC
  ( ByteString, fromChunks, length )
import qualified Data.ByteString.Char8 as BC( ByteString, null, pack )
import qualified Data.Map as M( Map, fromList, size, keys )
import qualified Data.IntMap as IM( IntMap, fromList )
import Skema.Types( IOPointType(..), IOPointDataType(..) )
import Skema.Util( hexByteString, byteStringHex )
import Skema.JSON( prettyJSON )
import Skema.ProgramFlow
  ( PFIOPoint(..), PFNode(..), PFKernel(..), PFArrow(..), ProgramFlow(..), 
    generateJSONString )
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Skema.JSON tests

\begin{code}
prop_prettyjson_length :: String -> Bool
prop_prettyjson_length st = length st <= (length . prettyJSON) st
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Skema.Programflow tests

\begin{code}
prop_jsonProgramFlow :: ProgramFlow -> Bool
prop_jsonProgramFlow pf = generateJSONString pf /= ""
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
myArgs :: Args
myArgs = stdArgs { maxSuccess=500 }
myCheck :: Testable prop => prop -> IO ()
myCheck = quickCheckWith myArgs
\end{code}

\begin{code}
tests :: [(String, IO ())]
tests = [
  ("Skema.Util: hex -> ByteString", myCheck prop_hexByteString),
  ("Skema.Util: hex <-> ByteString id", myCheck prop_hexByteString_ident),
  ("Skema.JSON: prettyJSON length", myCheck prop_prettyjson_length),
  ("Skema.ProgramFlow: ProgramFlow -> JSON", myCheck prop_jsonProgramFlow) 
 ]
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
