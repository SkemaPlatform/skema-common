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
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
-- | Module with the function relative to Program Flows.
module Skema.ProgramFlow
    ( 
      -- * Types
      PFNodeID, PFNodePoint, PFIOPoint(..), PFConstBuffer(..), PFKernel(..), 
      ProgramFlow(..), PFNode(..), PFArrow(..), IOPoint(..), 
      -- * Basic values
      emptyProgramFlow, exampleProgramFlow, exampleKernel, exampleKernelWC,
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
import Control.Applicative( pure, (<*>) )
import Control.Arrow( second )
import Control.Monad( mzero )
import Data.Aeson( FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?) )
import qualified Data.Aeson.Types as Aeson
import Data.Functor( (<$>) )
import Data.Maybe( mapMaybe, fromJust, isJust )
import Data.List( intercalate, elemIndex )
import Data.ByteString.Lazy.Char8( ByteString, pack )
import Data.Digest.Pure.SHA( sha256, bytestringDigest )
import qualified Data.IntMap as MI( empty, fromList, (!) )
import qualified Data.Map as M( Map, empty, fromList, assocs, lookup, (!) )
import Data.Text( Text )
import Skema.Types( IOPointType(..), IOPointDataType(..), dataTypeSize )
import Skema.SIDMap( SID(..), SIDMap, sidMapAssocs )
import Skema.Util( toJSONString, fromJSONString )

(.:/) :: (FromJSON a) => Aeson.Object -> (Text, a) -> Aeson.Parser a 
obj .:/ (key, val) = case M.lookup key obj of
  Nothing -> pure val
  Just v  -> parseJSON v
{-# INLINE (.:/) #-}

-- -----------------------------------------------------------------------------
-- | Node id type.
newtype PFNodeID = PFNodeID Int deriving( Show, Eq, Ord )
instance SID PFNodeID where
  toInt (PFNodeID a) = a
  fromInt = PFNodeID  

instance ToJSON PFNodeID where
  toJSON = toJSON . toInt
    
instance FromJSON PFNodeID where
  parseJSON (Aeson.Number n) = pure (fromInt . floor $ n)
  parseJSON _ = mzero  
                               
-- -----------------------------------------------------------------------------
-- | I/O point in a Program Flow.
data PFIOPoint = PFIOPoint
    { pfIOPDataType :: !IOPointDataType -- ^ data type of the point
    , pfIOPType :: !IOPointType  -- ^ type of the point [Input|Output]
    } deriving( Show, Eq )

instance ToJSON PFIOPoint where
  toJSON (PFIOPoint dt t) = object [ "data" .= dt, 
                                     "type" .= t ]
                            
instance FromJSON PFIOPoint where
  parseJSON (Aeson.Object v) = PFIOPoint <$>
                               v .: "data" <*> 
                               v .: "type"
  parseJSON _ = mzero  

-- -----------------------------------------------------------------------------
-- | Const Buffer in a Kernel.
data PFConstBuffer = PFConstBuffer
                     { pfcbDataType :: !IOPointDataType 
                       -- ^ data type of the const buffer
                     }
                   deriving( Show, Eq )
                     
instance ToJSON PFConstBuffer where
  toJSON (PFConstBuffer dt) = object [ "data" .= dt ]
                            
instance FromJSON PFConstBuffer where
  parseJSON (Aeson.Object v) = PFConstBuffer <$>
                               v .: "data"
  parseJSON _ = mzero  
  
-- -----------------------------------------------------------------------------
-- | Program Flow Kernel.
data PFKernel = PFKernel
    { pfkBody :: !String -- ^ OpenCL body of the kernel.
    , pfkIOPoints :: M.Map String PFIOPoint 
      -- ^ list of names I/O points or parameters
    , pfkConstBuffers :: M.Map String PFConstBuffer
      -- ^ list of const buffers
    , pfkWorkItems :: Maybe Int -- ^ number of work items
    } deriving( Show, Eq )

instance ToJSON PFKernel where
  toJSON (PFKernel body ps cs wi) 
    | (isJust wi) = object [ "body" .= body, 
                             "io" .= ps, 
                             "const" .= cs, 
                             "workitems" .= wi ]
    | otherwise = object [ "body" .= body, 
                           "io" .= ps,
                           "const" .= cs ]
                              
instance FromJSON PFKernel where
  parseJSON (Aeson.Object v) = PFKernel <$>
                               v .: "body" <*> 
                               v .: "io" <*>
                               v .:/ ("const", M.empty) <*>
                               v .:? "workitems"
  parseJSON _ = mzero  

-- -----------------------------------------------------------------------------
-- | Program Flow Node. A node is a instance of a Program Flow Kernel `PFKernel`.
data PFNode = PFNode
    { pfnIndex :: !String -- ^ name of the kernel.
    } deriving( Show, Eq )

instance ToJSON PFNode where
  toJSON (PFNode idx) = object [ "kernel" .= idx ]
  
instance FromJSON PFNode where
  parseJSON (Aeson.Object v) = PFNode <$> 
                               v .: "kernel"
  parseJSON _ = mzero  
  
-- -----------------------------------------------------------------------------
-- | Define one of the two end points of a arrow in the Program Flow.
type PFNodePoint = (PFNodeID,String)

-- | Program Flow arrow between two nodes.
data PFArrow = PFArrow
    { pfaOutput :: !PFNodePoint -- ^ end of the arrow
    , pfaInput :: !PFNodePoint -- ^ begin of the arrow
    } deriving( Show, Eq )

instance ToJSON PFArrow where
  toJSON (PFArrow outp inp) = object [ "output" .= outp, 
                                       "input" .= inp ]

instance FromJSON PFArrow where
  parseJSON (Aeson.Object v) = PFArrow <$>
                               v .: "output" <*>
                               v .: "input"
  parseJSON _ = mzero  

-- -----------------------------------------------------------------------------
-- | Program Flow.
data ProgramFlow = ProgramFlow
    { pfKernels :: M.Map String PFKernel -- ^ Kernels used in the Program Flow
    , pfNodes :: SIDMap PFNode -- ^ Nodes of the Program Flow
    , pfArrows :: [PFArrow] -- ^ arrows between nodes
    } deriving( Show, Eq )

instance ToJSON ProgramFlow where
  toJSON (ProgramFlow ks ns as ) = object [ "kernels" .= ks, 
                                            "nodes" .= ns, 
                                            "arrows" .= as ]

instance FromJSON ProgramFlow where
  parseJSON (Aeson.Object v) = ProgramFlow <$>
                               v .: "kernels" <*>
                               v .: "nodes" <*>
                               v .: "arrows"                         
  parseJSON _ = mzero  
  
-- -----------------------------------------------------------------------------
-- | Get the Program Flow Node using its id.
programFlowNode :: ProgramFlow -> PFNodeID -> PFNode
programFlowNode pf nidx = (pfNodes pf) MI.! (toInt nidx)

-- | Get the Program Flow Kernel using its name.
programFlowKernel :: ProgramFlow -> String -> PFKernel
programFlowKernel pf kidx = (pfKernels pf) M.! kidx

-- -----------------------------------------------------------------------------
-- | empty Program Flow.
emptyProgramFlow :: ProgramFlow
emptyProgramFlow = ProgramFlow M.empty MI.empty []

-- | simple example Kernel.
exampleKernel :: PFKernel
exampleKernel = PFKernel "int id = get_global_id(0); o1[id] = 2*i1[id];" 
              (M.fromList [
                  ("i1",PFIOPoint IOfloat4 InputPoint),
                  ("o1",PFIOPoint IOfloat4 OutputPoint)])
              M.empty
              Nothing

-- | simple example Kernel with const buffer.
exampleKernelWC :: PFKernel
exampleKernelWC = PFKernel "int id = get_global_id(0); o1[id] = 2*i1[id];" 
              (M.fromList [
                  ("i1",PFIOPoint IOfloat4 InputPoint),
                  ("o1",PFIOPoint IOfloat4 OutputPoint)])
              (M.fromList [
                  ("c1", PFConstBuffer IOfloat)])
              Nothing

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
                  ("o1",PFIOPoint IOfloat4 OutputPoint)])
              M.empty
              (Just 20))],
  pfNodes= MI.fromList [(0, PFNode "kuno"),
                        (1, PFNode "kuno"),
                        (2, PFNode "kdos")],
  pfArrows= [PFArrow (fromInt 0,"o1") (fromInt 1,"i1"), 
             PFArrow (fromInt 1,"o1") (fromInt 2,"i1")]
  }

-- -----------------------------------------------------------------------------
-- | Generate a JSON string from a Program Flow.
generateJSONString :: ProgramFlow -> String
generateJSONString = toJSONString

-- | Decode a JSON string to obtain a Program Flow.
decodeJSONString :: String -> Either String ProgramFlow
decodeJSONString cad = case fromJSONString cad of
  Just pf -> Right pf
  Nothing -> Left "error decoding json"

-- -----------------------------------------------------------------------------
-- | Obtain the SHA-256 value of a Program Flow. It use the JSON string of the
-- Program Flow to calculate the hash.
programFlowHash :: ProgramFlow -> ByteString    
programFlowHash = bytestringDigest.sha256 . pack . generateJSONString

-- -----------------------------------------------------------------------------
-- | Check if a Program Flow point is an Output point.
isOutPoint :: PFIOPoint -> Bool
isOutPoint = (==OutputPoint) . pfIOPType

-- | Check if a Program Flow point is an input point.
isInPoint :: PFIOPoint -> Bool
isInPoint = (==InputPoint) . pfIOPType

-- | Calculate the size in bytes of a IO point.
ioPointDataSize :: PFIOPoint -> Int
ioPointDataSize = dataTypeSize . pfIOPDataType

-- | Calculate the buffer size, less than a limit, for a whole number of
-- elements with a specific type.
ioPointBufferSize :: PFIOPoint -- ^ type of the buffer elements.
                     -> Int -- ^ limit of the buffer
                     -> Int
ioPointBufferSize point limit = (limit `div` elemSize) * elemSize
  where
    elemSize = ioPointDataSize point

-- | Calculate the number of whole elements that can be saved in a buffer
ioPointNumElems :: PFIOPoint -- ^ type of the buffer elements.
                   -> Int -- ^ size of the buffer
                   -> Int
ioPointNumElems point limit = limit `div` elemSize
  where
    elemSize = ioPointDataSize point

-- | IO point in a Kernel instance.
data IOPoint = IOPoint
    { iopType :: !IOPointType
    , iopDataType :: !IOPointDataType
    , iopNode :: ! PFNodeID
    , iopPoint :: !String}
               deriving( Show, Eq )

-- | Returns the Output points from a Program Flow kernel.
kernelOutputPoints :: PFKernel -> [(String,PFIOPoint)]
kernelOutputPoints = filter (isOutPoint.snd) . M.assocs . pfkIOPoints

-- | Returns the Input points from a Program Flow Kernel.
kernelInputPoints :: PFKernel -> [(String,PFIOPoint)]
kernelInputPoints = filter (isInPoint.snd) . M.assocs . pfkIOPoints

-- | Create a IO point using a template.
createIOPoint :: PFNodeID -- ^ id for the new `IOPoint`
                 -> (String, PFIOPoint) -- ^ name and template.
                 -> IOPoint
createIOPoint idx (n,p) = IOPoint (pfIOPType p) (pfIOPDataType p) idx n

-- | lookup in a Program Flow for a kernel using an instance of that kernel.
kernelLookup :: ProgramFlow 
                -> (a,PFNode) -- ^ Instance of the Kernel
                -> Maybe (a,PFKernel)
kernelLookup pf (k,n) = maybe Nothing (Just.((,) k)) (M.lookup key kernels)
  where 
    key = pfnIndex n
    kernels = (pfKernels pf)

-- | Get the output `IOPoints` of a Program Flow.
outputPoints :: ProgramFlow -> [IOPoint]
outputPoints pf = concatMap (\(i,xs) -> map (createIOPoint i) xs) . map (second kernelOutputPoints) . mapMaybe (kernelLookup pf)$ nodes
  where
    nodes = sidMapAssocs $ pfNodes pf

-- | Get the input `IOPoints` of a Program Flow.
inputPoints :: ProgramFlow -> [IOPoint]
inputPoints pf = concatMap (\(i,xs) -> map (createIOPoint i) xs) . map (second kernelInputPoints) . mapMaybe (kernelLookup pf)$ nodes
  where
    nodes = sidMapAssocs $ pfNodes pf

-- | Get the unasigned output `IOPoints` of a Program Flow.
unasignedOutputPoints :: ProgramFlow -> [IOPoint]
unasignedOutputPoints pf = filter (not.(`elem`arrows).extract) $ outputPoints pf
  where
    arrows = map pfaOutput $ pfArrows pf
    extract p = (iopNode p, iopPoint p)

-- | Get the unasigned input `IOPoints` of a Program Flow.
unasignedInputPoints :: ProgramFlow -> [IOPoint]
unasignedInputPoints pf = filter ((`notElem`arrows).extract) $ inputPoints pf
  where
    arrows = map pfaInput $ pfArrows pf
    extract p = (iopNode p, iopPoint p)

-- -----------------------------------------------------------------------------
-- | Get the output arrow from a arrow point in a Program Flow.
arrowFrom :: PFNodePoint -> ProgramFlow -> PFArrow
arrowFrom output pf = head . filter outfrom $ pfArrows pf
  where
    outfrom arr = (pfaOutput arr) == output

-- | Get the arrows that out from a node.
arrowsFromNode :: PFNodeID -> ProgramFlow -> [PFArrow]
arrowsFromNode nid pf = arrows
  where
    arrows = filter outfrom $ pfArrows pf
    outfrom arr = (fst $ pfaOutput arr) == nid

-- | Get the input arrow from a arrow point in a Program Flow.
arrowsToNode :: PFNodeID -> ProgramFlow -> [PFArrow]
arrowsToNode nid pf = arrows
  where
    arrows = filter into $ pfArrows pf
    into arr = (fst $ pfaInput arr) == nid

-- | Obtain the names of free out points from a node.
freeNodeOut :: PFNodeID -> ProgramFlow -> [String]
freeNodeOut nid pf = map fst outs
  where
    outs = filter check points
    points = M.assocs $ pfkIOPoints kernel
    kernel = (pfKernels pf) M.! (pfnIndex node)
    node = (pfNodes pf) MI.! (toInt nid)
    arrows = map (snd.pfaOutput) $ arrowsFromNode nid pf
    check p = ((`notElem`arrows).fst $ p)&&(isOutPoint.snd $ p)

-- | Obtain the names of bounded in points from a node.
boundedNodeIn :: PFNodeID -> ProgramFlow -> [String]
boundedNodeIn nid pf = map fst ins
  where
    ins = filter check points
    points = M.assocs $ pfkIOPoints kernel
    kernel = (pfKernels pf) M.! (pfnIndex node)
    node = (pfNodes pf) MI.! (toInt nid)
    arrows = map (snd.pfaInput) $ arrowsToNode nid pf
    check p = ((`elem`arrows).fst $ p)&&(isInPoint.snd $ p)

-- | Obtain the names of bounded out points from a node.
boundedNodeOut :: PFNodeID -> ProgramFlow -> [String]
boundedNodeOut nid pf = map fst ins
  where
    ins = filter check points
    points = M.assocs $ pfkIOPoints kernel
    kernel = (pfKernels pf) M.! (pfnIndex node)
    node = (pfNodes pf) MI.! (toInt nid)
    arrows = map (snd.pfaOutput) $ arrowsFromNode nid pf
    check p = ((`elem`arrows).fst $ p)&&(isOutPoint.snd $ p)

-- | get the index of a IO point in a node.
nodeIOpos :: PFNodeID -> String -> ProgramFlow -> Int
nodeIOpos nid name pf = fromJust $ elemIndex name names
  where
    names = map fst . M.assocs $ pfkIOPoints kernel
    kernel = (pfKernels pf) M.! (pfnIndex node)
    node = (pfNodes pf) MI.! (toInt nid)

-- | get the index of a IO point in a kernel.
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
    ioparams = map iopar $ M.assocs $ pfkIOPoints krn
    constparams = map constpar $ M.assocs $ pfkConstBuffers krn
    sizeparams = map sizepar $ M.assocs $ pfkConstBuffers krn
    parameters = intercalate ", " $ concat [ioparams, constparams, sizeparams]
    iopar (pn,t) = concat["__global ", show $ pfIOPDataType t, " *", pn]
    constpar (pn,t) = concat["__constant ", show $ pfcbDataType t, " *", pn]
    sizepar (pn,_) = concat["int sz_", pn]    

-- -----------------------------------------------------------------------------
