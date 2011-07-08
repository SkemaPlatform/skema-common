%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of Skema-Common.

% Skema-Common is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.

% Skema-Common is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.

% You should have received a copy of the GNU General Public License
% along with Skema-Common.  If not, see <http://www.gnu.org/licenses/>.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
-- | Useful types for Skema programs
module Skema.Types( IOPointType(..), IOPointDataType(..) ) where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Text.JSON( JSON(..), JSValue(..), Result(..), fromJSString )
import qualified Data.Map as M( Map, (!), fromList, lookup )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Swap is available in Base 4.3.1.* but not in 4.2.0.*
\begin{code}
-- | interchange the values of a tuple
swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
-- | Type of a comunication point of a node
data IOPointType = InputPoint -- ^ Input
                 | OutputPoint -- ^ Output
                   deriving( Show, Read, Eq, Enum, Bounded )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
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
\end{code}

\begin{code}
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
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
instance Show IOPointDataType where
  show v = dataTypeShowTable M.! v
\end{code}

\begin{code}
instance Read IOPointDataType where
  readsPrec _ r = do
    (k,t) <- lex r
    case M.lookup k dataTypeReadTable of
      Nothing -> []
      Just v -> [(v,t)]
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
instance JSON IOPointType where
    showJSON = showJSON . show
    readJSON (JSString v) = case (reads . fromJSString $ v) of
      [] -> Error "invalid string for IOPointType"
      (iot,_):_ -> Ok iot
    readJSON _ = Error "invalid value for IOPointType"
\end{code}

\begin{code}
instance JSON IOPointDataType where
    showJSON = showJSON . show
    readJSON (JSString v) = case (reads . fromJSString $ v) of
      [] -> Error "invalid string for IOPointDataType"
      (iot,_):_ -> Ok iot
    readJSON _ = Error "invalid value for IOPointDataType"
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
