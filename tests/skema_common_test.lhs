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
import Test.QuickCheck( Arbitrary(..), quickCheck )
import Text.Printf( printf )
import Data.Char( isHexDigit )
import qualified Data.ByteString.Lazy.Char8 as LC
  ( ByteString, fromChunks, length )
import qualified Data.ByteString.Char8 as BC( ByteString, null, pack )
import Skema.Util( hexByteString, byteStringHex )
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
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Skema.Util tests

\begin{code}
prop_hexByteString :: String -> Bool
prop_hexByteString xs = (LC.length.hexByteString) cad <= (fromIntegral.length) cad
  where
    cad = filter isHexDigit xs
\end{code}

\begin{code}
prop_hexByteString_ident :: LC.ByteString -> Bool
prop_hexByteString_ident xs = (hexByteString . byteStringHex) xs == xs
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
tests :: [(String, IO ())]
tests = [
  ("Skema.Util: hex -> ByteStrign", quickCheck prop_hexByteString),
  ("Skema.Util: hex <-> ByteStrign id", quickCheck prop_hexByteString_ident)
 ]
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
