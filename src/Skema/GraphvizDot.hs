{- -----------------------------------------------------------------------------
Copyright (C) 2011  Luis Cabellos - Instituto de Fisica de Cantabria
This file is part of Skema-Common.

Skema-Common is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

Skema-Common is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with Skema-Common.  If not, see <http://www.gnu.org/licenses/>.
-- ----------------------------------------------------------------------------}
module Skema.GraphvizDot( toGraphvizDot ) where

-- -----------------------------------------------------------------------------
import Control.Arrow( second, (&&&) )
import Data.Maybe( catMaybes )
import qualified Data.Map as M( lookup )
import Skema.ProgramFlow( 
  ProgramFlow(..), PFKernel(..), PFNode(..), PFArrow(..), PFIOPoint(..), 
  IOPoint(..), kernelInputPoints, kernelOutputPoints, unasignedInputPoints, 
  unasignedOutputPoints )
import Skema.SIDMap( toInt, sidMapAssocs )

-- -----------------------------------------------------------------------------
toGraphvizDot :: ProgramFlow -> String
toGraphvizDot pf = "digraph { rankdir=\"LR\";\n"
                   ++ (concat . fmap toGraphvizNode $ nodes)
                   ++ (concat . fmap toGraphvizArrow $ arrows)
                   ++ (concat . fmap toGraphvizStart $ startpoints)
                   ++ (concat . fmap toGraphvizEnd $ endpoints)
                   ++ "}\n"
  where
    nodes = catMaybes . fmap (kernelLookup pf)
            . sidMapAssocs $ pfNodes pf
    arrows =  pfArrows pf
    startpoints = zip [0..] . fmap (toInt . iopNode &&& iopPoint) 
                  $ unasignedInputPoints pf
    endpoints = zip [0..] . fmap (toInt . iopNode &&& iopPoint)
                $ unasignedOutputPoints pf

-- -----------------------------------------------------------------------------
toGraphvizNode :: (Int,String,PFKernel) -> String
toGraphvizNode (n,s,kern) = 
  show n ++ "[shape=plaintext, label=<"
  ++ "<TABLE BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"0\"><TR>"
  ++ "<TD bgcolor=\"grey30\">"
  ++ "<font color=\"white\" >" ++ s ++ "</font></TD>"
  ++ "<TD bgcolor=\"grey30\"></TD></TR>"
  ++ (concat opoints)
  ++ "<TR><TD></TD></TR><TR><TD></TD></TR>"
  ++ "<TR><TD></TD></TR><TR><TD></TD></TR>"
  ++ "<TR><TD></TD></TR><TR><TD></TD></TR>"
  ++ (concat ipoints)
  ++ "</TABLE>>];\n" 
    where
      ipoints = fmap (ipRow . second pfIOPDataType) $ kernelInputPoints kern
      ipRow (is,it) = 
        "<TR><TD PORT=\"" ++ is ++ "\" align=\"right\">" 
        ++ is ++ " [" ++ show it ++ "]</TD></TR>"
      opoints = fmap (opRow . second pfIOPDataType) $ kernelOutputPoints kern
      opRow (os,ot) = 
        "<TR><TD></TD><TD PORT=\"" ++ os ++ "\" align=\"left\">" 
        ++ os ++ " [" ++ show ot ++ "]</TD></TR>"
        
kernelLookup :: ProgramFlow 
                -> (a,PFNode) -- ^ Instance of the Kernel
                -> Maybe (a,String,PFKernel)
kernelLookup pf (k,n) = maybe Nothing (Just.(\a-> (k,key,a))) (M.lookup key kernels)
  where 
    key = pfnIndex n
    kernels = (pfKernels pf)

-- -----------------------------------------------------------------------------
toGraphvizArrow :: PFArrow -> String
toGraphvizArrow (PFArrow (aon,aos) (ain,ais)) = 
  (show . toInt $ aon)
  ++ ":" ++ aos ++ " -> "  
  ++ (show . toInt $ ain)
  ++ ":" ++ ais
  ++ "[dir=both, arrowhead=\"dot\", arrowtail=\"odot\"];\n"

-- -----------------------------------------------------------------------------
toGraphvizStart :: (Int, (Int, String)) -> String
toGraphvizStart (idx,(inode,is)) = 
  nodeName ++ " [shape=plaintext, label=\"\"]; "
  ++ nodeName ++ " -> " ++ show inode ++ ":" ++ is 
  ++ " [arrowhead=\"dot\", style=\"dashed\"];\n"
    where
      nodeName = "start" ++ show idx
      
-- -----------------------------------------------------------------------------
toGraphvizEnd :: (Int, (Int, String)) -> String
toGraphvizEnd (idx,(inode,is)) = 
  nodeName ++ " [shape=plaintext, label=\"\"]; "
  ++ show inode ++ ":" ++ is ++ " -> " ++ nodeName 
  ++ " [dir=back, arrowtail=\"odot\", style=\"dashed\"];\n"
    where
      nodeName = "end" ++ show idx

-- -----------------------------------------------------------------------------
