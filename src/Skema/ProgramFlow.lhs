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
    , IOPoint(..), emptyProgramFlow, exampleProgramFlow, generateJSONString
    , decodeJSONString, programFlowHash, outputPoints, inputPoints
    , unasignedOutputPoints, unasignedInputPoints ) 
    where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Control.Arrow( second )
import Data.Maybe( mapMaybe )
import Data.ByteString.Lazy.Char8( ByteString, pack )
import Data.Digest.Pure.SHA( sha256, bytestringDigest )
import qualified Data.IntMap as MI( IntMap, empty, fromList, assocs )
import qualified Data.Map as M( Map, empty, fromList, assocs, lookup )
import Text.JSON
    ( Result(..), JSON(..), JSValue(..), makeObj, encode, decode, fromJSObject )
import Skema.Types( IOPointType(..), IOPointDataType(..) )
import Skema.JSON( smapToObj, objToSmap, jsonLookup )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
data PFIOPoint = PFIOPoint
    { pfIOPDataType :: !IOPointDataType 
    , pfIOPType :: !IOPointType }
    deriving( Show, Eq )
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
    deriving( Show, Eq )
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
    deriving( Show, Eq )
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
               deriving( Show, Eq )
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
    deriving( Show, Eq )
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

\begin{code}
decodeJSONString :: String -> Either String ProgramFlow
decodeJSONString cad = case decode cad of
  Ok pf -> Right pf
  Error msg -> Left msg
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
programFlowHash :: ProgramFlow -> ByteString    
programFlowHash = bytestringDigest.sha256 . pack . generateJSONString
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
data IOPoint = IOPoint
    { iopType :: !IOPointType
    , iopDataType :: !IOPointDataType
    , iopNode :: !Int
    , iopPoint :: !String}
               deriving( Show, Eq )
\end{code}

\begin{code}
kernelOutputPoints :: PFKernel -> [(String,PFIOPoint)]
kernelOutputPoints = filter isOutput . M.assocs . pfkIOPoints
  where
    isOutput = (==OutputPoint).pfIOPType.snd
\end{code}

\begin{code}
kernelInputPoints :: PFKernel -> [(String,PFIOPoint)]
kernelInputPoints = filter isInput . M.assocs . pfkIOPoints
  where
    isInput = (==InputPoint).pfIOPType.snd
\end{code}

\begin{code}
createIOPoint :: Int -> (String, PFIOPoint) -> IOPoint
createIOPoint idx (n, p) = IOPoint (pfIOPType p) (pfIOPDataType p) idx n
\end{code}

\begin{code}
kernelLookup :: ProgramFlow -> (a,PFNode) -> Maybe (a,PFKernel)
kernelLookup pf (k,n) = maybe Nothing (Just.((,) k)) (M.lookup key kernels)
  where 
    key = pfnIndex n
    kernels = (pfKernels pf)
\end{code}

\begin{code}
outputPoints :: ProgramFlow -> [IOPoint]
outputPoints pf = concatMap (\(i,xs) -> map (createIOPoint i) xs) . map (second kernelOutputPoints) . mapMaybe (kernelLookup pf)$ nodes
  where
    nodes = MI.assocs $ pfNodes pf
\end{code}

\begin{code}
inputPoints :: ProgramFlow -> [IOPoint]
inputPoints pf = concatMap (\(i,xs) -> map (createIOPoint i) xs) . map (second kernelInputPoints) . mapMaybe (kernelLookup pf)$ nodes
  where
    nodes = MI.assocs $ pfNodes pf
\end{code}

\begin{code}
unasignedOutputPoints :: ProgramFlow -> [IOPoint]
unasignedOutputPoints pf = filter ((`elem`arrows).extract) $ outputPoints pf
  where
    arrows = map pfaOuput $ pfArrows pf
    extract p = (iopNode p, iopPoint p)
\end{code}

\begin{code}
unasignedInputPoints :: ProgramFlow -> [IOPoint]
unasignedInputPoints pf = filter ((`elem`arrows).extract) $ inputPoints pf
  where
    arrows = map pfaInput $ pfArrows pf
    extract p = (iopNode p, iopPoint p)
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
