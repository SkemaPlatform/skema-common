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
    , emptyProgramFlow, exampleProgramFlow, generateJSONString
    , programFlowHash ) where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Data.ByteString.Lazy.Char8( ByteString, pack )
import Data.Digest.Pure.SHA( sha256, bytestringDigest )
import qualified Data.IntMap as MI( IntMap, empty, fromList )
import qualified Data.Map as M( Map, empty, fromList )
import Text.JSON
    ( Result(..), JSON(..), JSValue(..), makeObj, encode, fromJSObject )
import Skema.Types( IOPointType(..), IOPointDataType(..) )
import Skema.JSON( smapToObj, objToSmap, jsonLookup )
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
    readJSON (JSObject obj) = let
      jsonObjAssoc = fromJSObject obj
      in do
        d <- jsonLookup "data" jsonObjAssoc >>= readJSON
        t <- jsonLookup "type" jsonObjAssoc >>= readJSON
        return $ PFIOPoint d t 
    readJSON _ = fail "invalid PFIOPoint"
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
                   , ("io", smapToObj . pfkIOPoints $ pfk)]
    readJSON (JSObject obj) = let
      jsonObjAssoc = fromJSObject obj
      in do
        body <- jsonLookup "body" jsonObjAssoc >>= readJSON
        io <- jsonLookup "io" jsonObjAssoc >>= objToSmap
        return $ PFKernel body io
    readJSON _ = Error "invalid PFKernel"
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
    readJSON (JSObject obj) = let
      jsonObjAssoc = fromJSObject obj
      in do
        i <- jsonLookup "kernel" jsonObjAssoc >>= readJSON
        return $ PFNode i
    readJSON _ = fail "invalid PFNode"
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
                   [ ("output", showJSON . pfaOuput $ pfa)
                   , ("input", showJSON . pfaInput $ pfa)]
    readJSON (JSObject obj) = let
      jsonObjAssoc = fromJSObject obj
      in do
        os <- jsonLookup "output" jsonObjAssoc >>= readJSON
        is <- jsonLookup "input" jsonObjAssoc >>= readJSON
        return $ PFArrow os is
    readJSON _ = fail "invalid PFArrow"
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
                   [ ("kernels", smapToObj . pfKernels $ pfn)
                   , ("nodes", showJSON . pfNodes $ pfn)
                   , ("arrows", showJSON . pfArrows $ pfn)]
    readJSON (JSObject obj) = let
      jsonObjAssoc = fromJSObject obj
      in do
        ks <- jsonLookup "kernels" jsonObjAssoc >>= objToSmap
        ns <- jsonLookup "nodes" jsonObjAssoc >>= readJSON
        as <- jsonLookup "arrows" jsonObjAssoc >>= readJSON
        return $ ProgramFlow ks ns as
    readJSON _ = Error "invalid ProgramFlow"
\end{code}

\begin{code}
emptyProgramFlow :: ProgramFlow
emptyProgramFlow = ProgramFlow M.empty MI.empty []
\end{code}

\begin{code}
exampleKernel :: PFKernel
exampleKernel = PFKernel "" 
              (M.fromList [
                  ("i1",PFIOPoint IOfloat4 InputPoint),
                  ("o1",PFIOPoint IOfloat4 OutputPoint)])

exampleProgramFlow :: ProgramFlow
exampleProgramFlow = emptyProgramFlow {
  pfKernels= M.fromList [
     ("kuno", exampleKernel),
     ("kdos", PFKernel "" 
              (M.fromList [
                  ("i1",PFIOPoint IOfloat4 InputPoint),
                  ("i2",PFIOPoint IOchar InputPoint),
                  ("i3",PFIOPoint IOlong InputPoint),
                  ("o1",PFIOPoint IOfloat4 OutputPoint)]))],
  pfNodes= MI.fromList [(0, PFNode "kuno"),
                        (1, PFNode "kuno"),
                        (2, PFNode "kdos")],
  pfArrows= [PFArrow (0,"o1") (1,"i1"), PFArrow (1,"o1") (2,"i1")]
  }
\end{code}

\begin{code}
generateJSONString :: ProgramFlow -> String
generateJSONString = encode . showJSON
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
programFlowHash :: ProgramFlow -> ByteString    
programFlowHash = bytestringDigest.sha256 . pack . generateJSONString
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%