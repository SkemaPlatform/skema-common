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
module Skema.Types( IOPointType(..), IOPointDataType(..) ) where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Text.JSON( JSON(..), Result(..), showJSON )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
data IOPointType = InputPoint
                 | OutputPoint
                   deriving( Show, Eq )
\end{code}

\begin{code}
data IOPointDataType = IOchar | IOuchar
                     | IOshort | IOushort
                     | IOint | IOuint
                     | IOlong | IOulong
                     | IOfloat
                     | IOchar2 | IOuchar2
                     | IOshort2 | IOushort2
                     | IOint2 | IOuint2
                     | IOlong2 | IOulong2
                     | IOfloat2
                     | IOchar4 | IOuchar4
                     | IOshort4 | IOushort4
                     | IOint4 | IOuint4
                     | IOlong4 | IOulong4
                     | IOfloat4
                     | IOchar8 | IOuchar8
                     | IOshort8 | IOushort8
                     | IOint8 | IOuint8
                     | IOlong8 | IOulong8
                     | IOfloat8
                     | IOchar16 | IOuchar16
                     | IOshort16 | IOushort16
                     | IOint16 | IOuint16
                     | IOlong16 | IOulong16
                     | IOfloat16
                     deriving( Eq )
\end{code}

\begin{code}
instance Show IOPointDataType where
  show IOchar = "char"
  show IOuchar = "uchar"
  show IOshort = "short"
  show IOushort = "ushort"
  show IOint = "int"
  show IOuint = "uint"
  show IOlong = "long"
  show IOulong = "ulong"
  show IOfloat = "float"
  show IOchar2 = "char2"
  show IOuchar2 = "uchar2"
  show IOshort2 = "short2"
  show IOushort2 = "ushort2"
  show IOint2 = "int2"
  show IOuint2 = "uint2"
  show IOlong2 = "long2"
  show IOulong2 = "ulong2"
  show IOfloat2 = "float2"
  show IOchar4 = "char4"
  show IOuchar4 = "uchar4"
  show IOshort4 = "short4"
  show IOushort4 = "ushort4"
  show IOint4 = "int4"
  show IOuint4 = "uint4"
  show IOlong4 = "long4"
  show IOulong4 = "ulong4"
  show IOfloat4 = "float4"
  show IOchar8 = "char8"
  show IOuchar8 = "uchar8"
  show IOshort8 = "short8"
  show IOushort8 = "ushort8"
  show IOint8 = "int8"
  show IOuint8 = "uint8"
  show IOlong8 = "long8"
  show IOulong8 = "ulong8"
  show IOfloat8 = "float8"
  show IOchar16 = "char16"
  show IOuchar16 = "uchar16"
  show IOshort16 = "short16"
  show IOushort16 = "ushort16"
  show IOint16 = "int16"
  show IOuint16 = "uint16"
  show IOlong16 = "long16"
  show IOulong16 = "ulong16"
  show IOfloat16 = "float16"
\end{code}

\begin{code}
instance JSON IOPointType where
    showJSON = showJSON . show
    readJSON _ = Error "not implemented"
\end{code}

\begin{code}
instance JSON IOPointDataType where
    showJSON = showJSON . show
    readJSON _ = Error "not implemented"
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
