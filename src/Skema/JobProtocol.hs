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
{-# LANGUAGE OverloadedStrings #-}
-- | Functions to work with the Job Protocol of Skema Platform.
module Skema.JobProtocol( 
  -- * Job Protocol Types
  JPJobID(..), JPJobList(..), JPJobRequest(..), JPJobInfo(..),
  -- * Job Protocol Functions
  sendSkemaJob
   )
       where

-- -----------------------------------------------------------------------------
import Control.Applicative( (<*>) )
import Control.Monad( mzero )
import Data.Aeson( FromJSON(..), ToJSON(..), object, (.=), (.:) )
import qualified Data.Aeson.Types as T
import Data.Functor( (<$>) )
import Network.HTTP( simpleHTTP, Response(..) )
import Skema.Network( postMultipartData, addBasicAuthorize )
import Skema.ProgramFlow( ProgramFlow )
import Skema.Util( toJSONString, fromJSONString )

-- -----------------------------------------------------------------------------
data JPJobID = JPJobID 
               { jobID :: ! Integer }
             deriving( Show )

instance ToJSON JPJobID where
  toJSON (JPJobID p) = object [ "jid" .= p ]
  
instance FromJSON JPJobID where
  parseJSON (T.Object v) = JPJobID <$>
                         v .: "jid"
  parseJSON _          = mzero
  
-- -----------------------------------------------------------------------------
data JPJobList = JPJobList 
                 { jobList :: [Integer] }
               deriving( Show )
                             
instance ToJSON JPJobList where
  toJSON (JPJobList ps) = object [ "jidList" .= ps ]
  
instance FromJSON JPJobList where
  parseJSON (T.Object v) = JPJobList <$>
                           v .: "jidList"
  parseJSON _          = mzero  
  
-- -----------------------------------------------------------------------------
data JPJobRequest = JPJobRequest
                    { jrSkemaProgram :: ! ProgramFlow,
                      jrInput :: [String], 
                      jrOutput :: [String] }
                  deriving( Show )
                          
instance ToJSON JPJobRequest where
  toJSON (JPJobRequest pg inp outp) = object [ "program" .= pg, 
                                         "inp" .= inp, 
                                         "outp" .= outp ]
  
instance FromJSON JPJobRequest where
  parseJSON (T.Object v) = JPJobRequest <$>
                           v .: "program" <*>
                           v .: "inp" <*>
                           v .: "outp"
  parseJSON _          = mzero  
                           
-- -----------------------------------------------------------------------------
data JPJobInfo = JPJobInfo
             { jiJobID :: ! Integer,
               jiUID :: ! Int,
               jiSkemaProgram :: ! ProgramFlow
             }

instance ToJSON JPJobInfo where
  toJSON (JPJobInfo jid uid pg) = object [ "jid" .= jid, 
                                           "uid" .= uid, 
                                           "program" .= pg ]
  
instance FromJSON JPJobInfo where
  parseJSON (T.Object v) = JPJobInfo <$>
                           v .: "jid" <*>
                           v .: "uid" <*>
                           v .: "program"
  parseJSON _          = mzero  
                           
-- -----------------------------------------------------------------------------
sendSkemaJob :: String -> JPJobRequest -> String -> IO (Maybe JPJobID)
sendSkemaJob url job user = do
  rq <- postMultipartData (url ++ "/jobs") $ toJSONString job
  rst <- simpleHTTP (addBasicAuthorize user rq)
  case rst of
    Left _ -> return Nothing
    Right a -> return . fromJSONString . rspBody $ a

-- -----------------------------------------------------------------------------
