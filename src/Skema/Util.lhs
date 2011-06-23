%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of Skema-Common.

% Skema-Common is free software: you can redistribute it and/or modify
% it under the terms of the GNU Affero General Public License as
% published by the Free Software Foundation, either version 3 of the
% License, or (at your option) any later version.

% Skema-Common is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Affero General Public License for more details.

% You should have received a copy of the GNU Affero General Public License
% along with Skema-Common.  If not, see <http://www.gnu.org/licenses/>.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
module Skema.Util( byteStringHex, hexByteString ) where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Data.ByteString.Lazy( ByteString, unpack, pack )
import Data.Bits( (.&.), (.|.), shiftR, shiftL )
import Data.Char( intToDigit, digitToInt )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
byteStringHex :: ByteString -> String
byteStringHex bs = foldr paddedShowHex [] . unpack $ bs
 where
   paddedShowHex x xs = intToDigit (fromIntegral (x `shiftR` 4))
                      : intToDigit (fromIntegral (x .&. 0xf))
                      : xs
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
hexByteString :: String -> ByteString
hexByteString = pack . map (fromInteger.toInteger).groupBinary . map digitToInt
  where
    groupBinary [] = []
    groupBinary (x:[]) = [x .&. 0xf]
    groupBinary (x:y:xs) = binaryAdd x y : groupBinary xs
    binaryAdd x y = ((x .&. 0xf) `shiftL` 4) .|. (y .&. 0xf)
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%