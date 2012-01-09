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
import Data.Maybe( catMaybes )
import qualified Data.Map as M( 
  Map, size, empty, fromList, elems, assocs, lookup, (!) )
import Skema.ProgramFlow( ProgramFlow(..), PFKernel(..), PFNode(..) )
import Skema.SIDMap( sidMapAssocs )

-- -----------------------------------------------------------------------------
toGraphvizDot :: ProgramFlow -> String
toGraphvizDot pf = "digraph { rankdir=\"LR\";\n"
                   ++ concat nodes
                   ++ "}\n"
  where
    nodes = fmap toGraphvizNode 
            . catMaybes . fmap (kernelLookup pf)
            . sidMapAssocs $ pfNodes pf

-- -----------------------------------------------------------------------------
toGraphvizNode :: (Int,String,PFKernel) -> String
toGraphvizNode (n,s,_) = show n 
                         ++ "[shape=plaintext, label=<<TABLE BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"0\"><TR><TD bgcolor=\"grey30\"><font color=\"white\" >"
                         ++ s
                         ++ "</font></TD><TD bgcolor=\"grey30\"></TD></TR>"
                         ++ "</TABLE>>];\n" 
{-
<TABLE BORDER="1" CELLBORDER="0" CELLSPACING="0">
  <TR><TD bgcolor="grey30"><font color="white" >fan</font></TD><TD bgcolor="grey30"></TD></TR>
  <TR><TD></TD><TD PORT="fan_y" align="left">y [float]</TD></TR>
  <TR><TD></TD><TD PORT="fan_x" align="left">x [float]</TD></TR>
  <TR><TD></TD></TR><TR><TD></TD></TR>
  <TR><TD></TD></TR><TR><TD></TD></TR>
  <TR><TD></TD></TR><TR><TD></TD></TR>
  <TR><TD></TD></TR>
  <TR><TD PORT="fan_z">z [float]</TD></TR>
</TABLE>>
-}

kernelLookup :: ProgramFlow 
                -> (a,PFNode) -- ^ Instance of the Kernel
                -> Maybe (a,String,PFKernel)
kernelLookup pf (k,n) = maybe Nothing (Just.(\a-> (k,key,a))) (M.lookup key kernels)
  where 
    key = pfnIndex n
    kernels = (pfKernels pf)

-- -----------------------------------------------------------------------------
