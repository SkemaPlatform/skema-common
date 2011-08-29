-- -----------------------------------------------------------------------------
-- This file is part of Skema-Common.

-- Skema-Common is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.

-- Skema-Common is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with Skema-Common.  If not, see <http://www.gnu.org/licenses/>.
-- -----------------------------------------------------------------------------
-- | Module with the function relative to Program Flows.
module Skema.ProgramFlow
    ( 
      -- * Types
      PFNodeID, PFArrowPoint, PFIOPoint(..), PFKernel(..), ProgramFlow(..), 
      PFNode(..), PFArrow(..), IOPoint(..), 
      -- * Basic values
      emptyProgramFlow, exampleProgramFlow, 
      -- * Convertion functions
      generateJSONString, decodeJSONString, programFlowHash, openclKernelSource,
      -- * Utility functions
      kernelInputPoints, kernelOutputPoints, programFlowNode, programFlowKernel,
      outputPoints, inputPoints, unasignedOutputPoints, unasignedInputPoints, 
      arrowFrom, arrowsFromNode, arrowsToNode, freeNodeOut, boundedNodeIn, 
      boundedNodeOut, nodeIOpos, kernelIOPos, ioPointDataSize, 
      ioPointBufferSize, ioPointNumElems ) 
    where

-- -----------------------------------------------------------------------------
import Control.Arrow( second, first )
import Data.Maybe( mapMaybe, fromJust )
import Data.List( intercalate, elemIndex )
import Data.ByteString.Lazy.Char8( ByteString, pack )
import Data.Digest.Pure.SHA( sha256, bytestringDigest )
import qualified Data.IntMap as MI( empty, fromList, (!) )
import qualified Data.Map as M( Map, empty, fromList, assocs, lookup, (!) )
import Text.JSON
    ( Result(..), JSON(..), JSValue(..), makeObj, encode, decode, fromJSObject )
import Skema.Types( IOPointType(..), IOPointDataType(..), dataTypeSize )
import Skema.JSON( smapToObj, objToSmap, jsonLookup )
import Skema.SIDMap( SID(..), SIDMap, sidMapAssocs )

-- -----------------------------------------------------------------------------
newtype PFNodeID = PFNodeID Int deriving( Show, Eq, Ord )
instance SID PFNodeID where
  toInt (PFNodeID a) = a
  fromInt = PFNodeID  

-- | I/O point in a Program Flow.
data PFIOPoint = PFIOPoint
    { pfIOPDataType :: !IOPointDataType -- ^ data type of the point
    , pfIOPType :: !IOPointType  -- ^ type of the point [Input|Output]
    } deriving( Show, Eq )

-- | Program Flow Kernel.
data PFKernel = PFKernel
    { pfkBody :: !String -- ^ OpenCL body of the kernel.
    , pfkIOPoints :: M.Map String PFIOPoint 
      -- ^ list of names I/O points or parameters
    } deriving( Show, Eq )

-- | Program Flow Node. A node is a instance of a Program Flow Kernel `PFKernel`.
data PFNode = PFNode
    { pfnIndex :: !String -- ^ name of the kernel.
    } deriving( Show, Eq )

-- | Define one of the two end points of a arrow in the Program Flow.
type PFArrowPoint = (PFNodeID,String)

-- | Program Flow arrow between two nodes.
data PFArrow = PFArrow
    { pfaOutput :: !PFArrowPoint -- ^ end of the arrow
    , pfaInput :: !PFArrowPoint -- ^ begin of the arrow
    } deriving( Show, Eq )

-- | Program Flow.
data ProgramFlow = ProgramFlow
    { pfKernels :: M.Map String PFKernel -- ^ Kernels used in the Program Flow
    , pfNodes :: SIDMap PFNode -- ^ Nodes of the Program Flow
    , pfArrows :: [PFArrow] -- ^ arrows between nodes
    } deriving( Show, Eq )

-- -----------------------------------------------------------------------------
programFlowNode :: ProgramFlow -> PFNodeID -> PFNode
programFlowNode pf nidx = (pfNodes pf) MI.! (toInt nidx)

programFlowKernel :: ProgramFlow -> String -> PFKernel
programFlowKernel pf kidx = (pfKernels pf) M.! kidx

-- -----------------------------------------------------------------------------
-- | empty Program Flow.
emptyProgramFlow :: ProgramFlow
emptyProgramFlow = ProgramFlow M.empty MI.empty []

exampleKernel :: PFKernel
exampleKernel = PFKernel "int id = get_global_id(0); o1[id] = 2*i1[id];" 
              (M.fromList [
                  ("i1",PFIOPoint IOfloat4 InputPoint),
                  ("o1",PFIOPoint IOfloat4 OutputPoint)])

-- | simple example Program Flow.
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
  pfArrows= [PFArrow (fromInt 0,"o1") (fromInt 1,"i1"), 
             PFArrow (fromInt 1,"o1") (fromInt 2,"i1")]
  }

-- -----------------------------------------------------------------------------
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

instance JSON PFNode where
    showJSON pfn = makeObj
                   [ ("kernel", showJSON . pfnIndex $ pfn )]
    readJSON (JSObject obj) = let
      jsonObjAssoc = fromJSObject obj
      in do
        i <- jsonLookup "kernel" jsonObjAssoc >>= readJSON
        return $ PFNode i
    readJSON _ = fail "invalid PFNode"

instance JSON PFArrow where
    showJSON pfa = makeObj
                   [ ("output", showJSON . first toInt . pfaOutput $ pfa)
                   , ("input", showJSON . first toInt . pfaInput $ pfa)]
    readJSON (JSObject obj) = let
      jsonObjAssoc = fromJSObject obj
      in do
        os <- jsonLookup "output" jsonObjAssoc >>= fmap (first fromInt) . readJSON
        is <- jsonLookup "input" jsonObjAssoc >>= fmap (first fromInt) . readJSON
        return $ PFArrow os is
    readJSON _ = fail "invalid PFArrow"

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

-- | Generate a JSON string from a Program Flow.
generateJSONString :: ProgramFlow -> String
generateJSONString = encode . showJSON

-- | Decode a JSON string to obtain a Program Flow.
decodeJSONString :: String -> Either String ProgramFlow
decodeJSONString cad = case decode cad of
  Ok pf -> Right pf
  Error msg -> Left msg

-- -----------------------------------------------------------------------------
-- | Obtain the SHA-256 value of a Program Flow. It use the JSON string of the
-- Program Flow to calculate the hash.
programFlowHash :: ProgramFlow -> ByteString    
programFlowHash = bytestringDigest.sha256 . pack . generateJSONString

-- -----------------------------------------------------------------------------
isOutPoint :: PFIOPoint -> Bool
isOutPoint = (==OutputPoint) . pfIOPType

isInPoint :: PFIOPoint -> Bool
isInPoint = (==InputPoint) . pfIOPType

ioPointDataSize :: PFIOPoint -> Int
ioPointDataSize = dataTypeSize . pfIOPDataType

ioPointBufferSize :: PFIOPoint -> Int -> Int
ioPointBufferSize point limit = (limit `div` elemSize) * elemSize
  where
    elemSize = ioPointDataSize point

ioPointNumElems :: PFIOPoint -> Int -> Int
ioPointNumElems point limit = limit `div` elemSize
  where
    elemSize = ioPointDataSize point

data IOPoint = IOPoint
    { iopType :: !IOPointType
    , iopDataType :: !IOPointDataType
    , iopNode :: ! PFNodeID
    , iopPoint :: !String}
               deriving( Show, Eq )

kernelOutputPoints :: PFKernel -> [(String,PFIOPoint)]
kernelOutputPoints = filter (isOutPoint.snd) . M.assocs . pfkIOPoints

kernelInputPoints :: PFKernel -> [(String,PFIOPoint)]
kernelInputPoints = filter (isInPoint.snd) . M.assocs . pfkIOPoints

createIOPoint :: PFNodeID -> (String, PFIOPoint) -> IOPoint
createIOPoint idx (n,p) = IOPoint (pfIOPType p) (pfIOPDataType p) idx n

kernelLookup :: ProgramFlow -> (a,PFNode) -> Maybe (a,PFKernel)
kernelLookup pf (k,n) = maybe Nothing (Just.((,) k)) (M.lookup key kernels)
  where 
    key = pfnIndex n
    kernels = (pfKernels pf)

outputPoints :: ProgramFlow -> [IOPoint]
outputPoints pf = concatMap (\(i,xs) -> map (createIOPoint i) xs) . map (second kernelOutputPoints) . mapMaybe (kernelLookup pf)$ nodes
  where
    nodes = sidMapAssocs $ pfNodes pf

inputPoints :: ProgramFlow -> [IOPoint]
inputPoints pf = concatMap (\(i,xs) -> map (createIOPoint i) xs) . map (second kernelInputPoints) . mapMaybe (kernelLookup pf)$ nodes
  where
    nodes = sidMapAssocs $ pfNodes pf

unasignedOutputPoints :: ProgramFlow -> [IOPoint]
unasignedOutputPoints pf = filter (not.(`elem`arrows).extract) $ outputPoints pf
  where
    arrows = map pfaOutput $ pfArrows pf
    extract p = (iopNode p, iopPoint p)

unasignedInputPoints :: ProgramFlow -> [IOPoint]
unasignedInputPoints pf = filter ((`notElem`arrows).extract) $ inputPoints pf
  where
    arrows = map pfaInput $ pfArrows pf
    extract p = (iopNode p, iopPoint p)

-- -----------------------------------------------------------------------------
arrowFrom :: PFArrowPoint -> ProgramFlow -> PFArrow
arrowFrom output pf = head . filter outfrom $ pfArrows pf
  where
    outfrom arr = (pfaOutput arr) == output

arrowsFromNode :: PFNodeID -> ProgramFlow -> [PFArrow]
arrowsFromNode nid pf = arrows
  where
    arrows = filter outfrom $ pfArrows pf
    outfrom arr = (fst $ pfaOutput arr) == nid

arrowsToNode :: PFNodeID -> ProgramFlow -> [PFArrow]
arrowsToNode nid pf = arrows
  where
    arrows = filter into $ pfArrows pf
    into arr = (fst $ pfaInput arr) == nid

freeNodeOut :: PFNodeID -> ProgramFlow -> [String]
freeNodeOut nid pf = map fst outs
  where
    outs = filter check points
    points = M.assocs $ pfkIOPoints kernel
    kernel = (pfKernels pf) M.! (pfnIndex node)
    node = (pfNodes pf) MI.! (toInt nid)
    arrows = map (snd.pfaOutput) $ arrowsFromNode nid pf
    check p = ((`notElem`arrows).fst $ p)&&(isOutPoint.snd $ p)

boundedNodeIn :: PFNodeID -> ProgramFlow -> [String]
boundedNodeIn nid pf = map fst ins
  where
    ins = filter check points
    points = M.assocs $ pfkIOPoints kernel
    kernel = (pfKernels pf) M.! (pfnIndex node)
    node = (pfNodes pf) MI.! (toInt nid)
    arrows = map (snd.pfaInput) $ arrowsToNode nid pf
    check p = ((`elem`arrows).fst $ p)&&(isInPoint.snd $ p)

boundedNodeOut :: PFNodeID -> ProgramFlow -> [String]
boundedNodeOut nid pf = map fst ins
  where
    ins = filter check points
    points = M.assocs $ pfkIOPoints kernel
    kernel = (pfKernels pf) M.! (pfnIndex node)
    node = (pfNodes pf) MI.! (toInt nid)
    arrows = map (snd.pfaOutput) $ arrowsFromNode nid pf
    check p = ((`elem`arrows).fst $ p)&&(isOutPoint.snd $ p)

nodeIOpos :: PFNodeID -> String -> ProgramFlow -> Int
nodeIOpos nid name pf = fromJust $ elemIndex name names
  where
    names = map fst . M.assocs $ pfkIOPoints kernel
    kernel = (pfKernels pf) M.! (pfnIndex node)
    node = (pfNodes pf) MI.! (toInt nid)

kernelIOPos :: PFKernel -> String -> Int
kernelIOPos kernel name = fromJust $ elemIndex name names
  where
    names = map fst . M.assocs $ pfkIOPoints kernel

-- -----------------------------------------------------------------------------
-- | Obtain the OpenCL source code of a named Kernel.
openclKernelSource :: String -> PFKernel -> String
openclKernelSource name krn = concat ["__kernel void ", name, 
                                      "( ", parameters, " ){\n", 
                                      pfkBody krn, "\n}"]
  where
    parameters = intercalate ", " $ map parameter $ M.assocs $ pfkIOPoints krn
    parameter (pn,t) = concat["__global ", show $ pfIOPDataType t, " *", pn]

-- -----------------------------------------------------------------------------
