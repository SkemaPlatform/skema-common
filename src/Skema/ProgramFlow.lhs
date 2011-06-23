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
module Skema.ProgramFlow
    ( PFIOPoint(..), PFKernel(..), ProgramFlow(..), PFNode(..), PFArrow(..)
    , emptyProgramFlow, generateJSONString ) where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Control.Arrow( second )
import qualified Data.IntMap as MI( IntMap, empty )
import qualified Data.Map as M( Map, empty, assocs )
import Text.JSON
    ( Result(..), JSON(..), JSValue(..), makeObj, encode )
import Skema.Types( IOPointType(..), IOPointDataType(..) )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
data PFIOPoint = PFIOPoint
    { pfIOPDataType :: !IOPointDataType 
    , pfIOPType :: !IOPointType }
    deriving( Show )
\end{code}

\begin{code}
instance JSON PFIOPoint where
    showJSON pfiop = makeObj 
                     [ ("data", (showJSON . pfIOPDataType) pfiop )
                     , ("type", (showJSON . pfIOPType) pfiop)]
    readJSON _ = Error "not implemented"
\end{code}

\begin{code}
data PFKernel = PFKernel
    { pfkBody :: !String
    , pfkIOPoints :: M.Map String PFIOPoint }
    deriving( Show )
\end{code}

\begin{code}
instance JSON PFKernel where
    showJSON pfk = makeObj 
                   [ ("body", showJSON . pfkBody $ pfk)
                   , ("io", mapToObj . pfkIOPoints $ pfk)]
    readJSON _ = Error "not implemented"
\end{code}

\begin{code}
data PFNode = PFNode
    { pfnIndex :: !String }
    deriving( Show )
\end{code}

\begin{code}
instance JSON PFNode where
    showJSON pfn = makeObj
                   [ ("kernel", showJSON . pfnIndex $ pfn )]
    readJSON _ = Error "not implemented"
\end{code}

\begin{code}
type PFArrowPoint = (Int,String)
\end{code}

\begin{code}
data PFArrow = PFArrow
    { pfaOuput :: !PFArrowPoint 
    , pfaInput :: !PFArrowPoint }
               deriving( Show )
\end{code}

\begin{code}
instance JSON PFArrow where
    showJSON pfa = makeObj
                   [ ("ouput", showJSON . pfaOuput $ pfa)
                   , ("input", showJSON . pfaInput $ pfa)]
    readJSON _ = Error "not implemented"
\end{code}

\begin{code}
data ProgramFlow = ProgramFlow
    { pfKernels :: M.Map String PFKernel
    , pfNodes :: MI.IntMap PFNode 
    , pfArrows :: [PFArrow]}
    deriving( Show )
\end{code}

\begin{code}
instance JSON ProgramFlow where
    showJSON pfn = makeObj
                   [ ("kernels", mapToObj . pfKernels $ pfn)
                   , ("nodes", showJSON . pfNodes $ pfn)
                   , ("arrows", showJSON . pfArrows $ pfn)]
    readJSON _ = Error "not implemented"
\end{code}

\begin{code}
mapToObj :: JSON a => M.Map String a -> JSValue
mapToObj = makeObj . map (second showJSON) . M.assocs
\end{code}

\begin{code}
emptyProgramFlow :: ProgramFlow
emptyProgramFlow = ProgramFlow M.empty MI.empty []
\end{code}

\begin{code}
generateJSONString :: ProgramFlow -> String
generateJSONString = encode . showJSON
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%