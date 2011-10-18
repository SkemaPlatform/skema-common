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
-- | Functions to work with the Job Protocol of Skema Platform.
module Skema.JobProtocol( 
  -- * Job Protocol Types
  JPJobID(..), JPJobList(..)
  -- * Job Protocol Functions
   )
       where

-- -----------------------------------------------------------------------------
import Control.Monad( mzero )
import Data.Aeson( FromJSON(..), ToJSON(..), object, (.=), (.:) )
import qualified Data.Aeson.Types as T
import Data.Functor( (<$>) )
import Data.Text( pack )

-- -----------------------------------------------------------------------------
data JPJobID = JPJobID 
               { jobID :: ! Integer }
             deriving( Show )

instance ToJSON JPJobID where
  toJSON (JPJobID p) = object [pack "jid" .= p]
  
instance FromJSON JPJobID where
  parseJSON (T.Object v) = JPJobID <$>
                         v .: pack "jid"
  parseJSON _          = mzero
  
-- -----------------------------------------------------------------------------
data JPJobList = JPJobList 
                 { jobList :: [Integer] }
               deriving( Show )
                             
instance ToJSON JPJobList where
  toJSON (JPJobList ps) = object [pack "jidList" .= ps]
  
instance FromJSON JPJobList where
  parseJSON (T.Object v) = JPJobList <$>
                           v .: pack "jidList"
  parseJSON _          = mzero  
  
-- -----------------------------------------------------------------------------
