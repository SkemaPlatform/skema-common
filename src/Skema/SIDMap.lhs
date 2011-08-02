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
-- | Classes and functions to work with a Map indexed with a SID type
module Skema.SIDMap(
  -- Types
  SID(..), SIDMap,
  -- Functions 
  sidMapAssocs, sidMapFromList, sidMapLookup ) 
       where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Control.Arrow( first )
import qualified Data.IntMap as MI( IntMap, assocs, fromList, lookup )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
class SID a where
  toInt :: a -> Int
  fromInt :: Int -> a
  fromSID :: SID b => b -> a
  fromSID = fromInt . toInt
  
instance SID Int where
  toInt = id
  fromInt = id
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
type SIDMap a = MI.IntMap a
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
sidMapAssocs :: SID k => SIDMap a -> [(k,a)]
sidMapAssocs = map (first fromInt) . MI.assocs
\end{code}

\begin{code}
sidMapFromList :: SID k => [(k,a)] -> SIDMap a
sidMapFromList = MI.fromList . map (first toInt)
\end{code}

\begin{code}
sidMapLookup ::SID k => k -> SIDMap a -> Maybe a
sidMapLookup k = MI.lookup (toInt k)
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
