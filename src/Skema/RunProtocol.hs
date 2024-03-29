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
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Functions to work with the Run Protocol of Skema Platform.
module Skema.RunProtocol( 
  -- * Run Protocol Types
  ServerPort(..), RPSkemaProgramID(..), RPProgramList(..), RPProgramRun(..), 
  RPRunIO(..),
  -- * Run Protocol Functions
  runBuffers, runBuffersCB, sendSkemaProgram, createSkemaRun )
       where

-- -----------------------------------------------------------------------------
import Control.Applicative( pure, (<*>) )
import Control.Arrow( second )
import Control.Concurrent( forkIO )
import Control.Concurrent.MVar( 
  MVar, putMVar, takeMVar, newMVar, modifyMVar_, withMVar, readMVar )
import Control.Monad( forM_, mzero )
import Control.Exception( finally )
import Data.Functor( (<$>) )
import qualified Data.ByteString as BS( ByteString, empty, null, append )
import qualified Data.ByteString.Lazy.Char8 as BSCL( ByteString )
import Data.Aeson( FromJSON(..), ToJSON(..), object, (.=), (.:) )
import qualified Data.Aeson.Types as T
import Network.HTTP( Response(..), simpleHTTP )
import Network.Socket( 
  PortNumber, SocketType(..), AddrInfo(..), AddrInfoFlag(..), Family(..), 
  socket, sClose, defaultHints, withSocketsDo, connect, getAddrInfo, 
  defaultProtocol )
import Network.Socket.ByteString( sendAll, recv )
import Skema.Network( postMultipartData, postFormUrlEncoded )
import Skema.ProgramFlow( 
  ProgramFlow, PFNodePoint, generateJSONString )
import Skema.Concurrent( 
  ChildLocks, newChildLocks, newChildLock, endChildLock, waitForChildren )
import Skema.Util( fromJSONString, toJSONString, b64ByteString, byteStringB64 )

-- -----------------------------------------------------------------------------
-- | Location of service.
data ServerPort = ServerPort
                  { spHostName :: ! String -- ^ Service Host name.
                  , spPort :: ! Int -- ^ Service Port. 
                  }
  
-- -----------------------------------------------------------------------------
webhost :: ServerPort -> String
webhost rs = "http://"++ spHostName rs ++ ":" ++ (show $ spPort rs)

-- -----------------------------------------------------------------------------
newServerPort :: ServerPort -> PortNumber -> ServerPort
newServerPort server p = server { spPort = fromIntegral p }

-- -----------------------------------------------------------------------------
desiredAddr :: Maybe AddrInfo
desiredAddr = Just $ defaultHints {
  addrSocketType = Stream,
  addrFamily = AF_INET, 
  addrFlags = [AI_PASSIVE]}

getServerAddrInfo :: ServerPort -> IO [AddrInfo]
getServerAddrInfo server = getAddrInfo desiredAddr 
                           (Just $ spHostName server) 
                           (Just . show $ spPort server)
  
-- -----------------------------------------------------------------------------
data RPError = RPConnError | RPServerError | RPClientError 
             | RPInternalServerError
             deriving( Show )

-- -----------------------------------------------------------------------------
-- | Skema Program ID.
data RPSkemaProgramID = RPSkemaProgramID 
                        { skemaProgramID :: ! BSCL.ByteString }
                      deriving( Show )

instance ToJSON RPSkemaProgramID where
  toJSON (RPSkemaProgramID p) = object [ "pid" .= p ]
  
instance FromJSON RPSkemaProgramID where
  parseJSON (T.Object v) = RPSkemaProgramID <$>
                           v .: "pid"
  parseJSON _          = mzero  
  
-- -----------------------------------------------------------------------------
-- | List of Skema Program IDs.
data RPProgramList = RPProgramList 
                     { programList :: [BSCL.ByteString] }
                     deriving( Show )
                             
instance ToJSON RPProgramList where
  toJSON (RPProgramList ps) = object [ "pidList" .= ps ]
  
instance FromJSON RPProgramList where
  parseJSON (T.Object v) = RPProgramList <$>
                           v .: "pidList"
  parseJSON _          = mzero  
  
-- -----------------------------------------------------------------------------
-- | Skema Program Run Request.
data RPProgramRun = RPProgramRun
                    { programID :: ! BSCL.ByteString 
                      -- ^ ID of Skema Program to Run.
                    , programConstData :: [(PFNodePoint, BS.ByteString)] 
                      -- ^ Const Data of the Run Instance.
                    }
                  deriving( Show )
  
encodeB64 :: [(PFNodePoint, BS.ByteString)] -> [(PFNodePoint, String)]
encodeB64 = map (second byteStringB64)

decodeB64 :: [(PFNodePoint, String)] -> [(PFNodePoint, BS.ByteString)]
decodeB64 = map (second b64ByteString)

instance ToJSON RPProgramRun where
  toJSON (RPProgramRun pid cs) = object [ "pid" .= pid
                                        , "cbuffs" .= encodeB64 cs ]
  
instance FromJSON RPProgramRun where
  parseJSON (T.Object v) = RPProgramRun <$>
                           v .: "pid" <*>
                           (fmap decodeB64 $ v .: "cbuffs")
  parseJSON _          = mzero  
  
-- -----------------------------------------------------------------------------
parseIntegral :: Integral a => T.Value -> T.Parser a
parseIntegral (T.Number n) = pure (floor n)
parseIntegral v          = T.typeMismatch "Integral" v

instance ToJSON PortNumber where
    toJSON = T.Number . fromIntegral
    {-# INLINE toJSON #-}

instance FromJSON PortNumber where
    parseJSON = parseIntegral
    {-# INLINE parseJSON #-}

-- | Skema Run Instance Communication Ports.
data RPRunIO = RPRunIO
               { inPorts :: [(PFNodePoint, PortNumber)]
               , outPorts :: [(PFNodePoint, PortNumber)] }
             deriving( Show )
                     
instance ToJSON RPRunIO where
  toJSON (RPRunIO is os) = object [ "ins" .= is, "outs" .= os ]
  
instance FromJSON RPRunIO where
  parseJSON (T.Object v) = RPRunIO <$>
                           v .: "ins" <*>
                           v .: "outs"
  parseJSON _          = mzero  

-- -----------------------------------------------------------------------------
-- | Send Skema Program to Skema Server. Returns the `RPSkemaProgramID` for use
-- that program.
sendSkemaProgram :: ServerPort -> ProgramFlow 
                    -> IO (Either RPError RPSkemaProgramID)
sendSkemaProgram server pf = do
  let skmData = generateJSONString pf
  catch 
    (do
        rq <- postMultipartData (webhost server ++ "/programs") skmData
        rst <- simpleHTTP rq
        case rst of
          Left _ -> return $ Left RPServerError
          Right a -> case rspCode a of
            (2, 0, 0) -> return $ maybe (Left RPServerError) 
                         Right (fromJSONString . rspBody $ a)
            (4, _, _) -> return $ Left RPClientError
            (5, _, _) -> return $ Left RPInternalServerError
            _ -> return $ Left RPServerError
    )
    (\_ -> return $ Left RPConnError
    )

-- -----------------------------------------------------------------------------
-- | Create a Run Instance of a Program. Returns the ports opened for
-- communication of data.
createSkemaRun :: ServerPort -> RPProgramRun -> IO (Either RPError RPRunIO)
createSkemaRun server prun = do
  catch 
    (do
        rq <- postFormUrlEncoded (webhost server ++ "/runs") 
              [("data",toJSONString prun)]
        rst <- simpleHTTP rq
        case rst of
          Left _ -> return $ Left RPServerError
          Right a -> case rspCode a of
            (2, 0, 0) -> return $ maybe (Left RPServerError)
                         (Right)
                         (fromJSONString . rspBody $ a) 
            (4, _, _) -> return $ Left RPClientError
            (5, _, _) -> return $ Left RPInternalServerError
            _ -> return $ Left RPServerError
    )
    (\_ -> return $ Left RPConnError
    )
    
-- -----------------------------------------------------------------------------
sendBuffer :: ServerPort -> BS.ByteString -> IO Bool
sendBuffer serverport datastream = withSocketsDo $ do
  addrinfos <- getServerAddrInfo serverport
  if null addrinfos
    then do
      return False
    else do
      let serveraddr = head addrinfos
      sock <- socket (addrFamily serveraddr) Stream defaultProtocol
      connect sock (addrAddress serveraddr)
      sendAll sock datastream
      sClose sock
      return True
      
sendAsyncBuffer :: ChildLocks -> ServerPort -> BS.ByteString
                   -> MVar Bool -> IO ()
sendAsyncBuffer children serverport b ret = do
  lock <- newChildLock children
  _ <- forkIO $ do
    val <- sendBuffer serverport b
    modifyMVar_ ret (\_ -> return val)
         `finally` endChildLock lock
  return ()
  
sendBufferInputs :: ChildLocks -> ServerPort -> [PortNumber] -> [BS.ByteString] 
                -> MVar [MVar Bool] -> IO ()
sendBufferInputs children server ports bs rets = do
  forM_ (zip ports bs) $ \(p,f) -> do
    ret <- newMVar False
    retvals <- takeMVar rets
    putMVar rets (ret:retvals)
    sendAsyncBuffer children (newServerPort server p) f ret

-- -----------------------------------------------------------------------------
getBuffer :: ServerPort -> IO BS.ByteString
getBuffer serverport = withSocketsDo $ do
  addrinfos <- getServerAddrInfo serverport
  if null addrinfos
    then do
      return BS.empty
    else do
      let serveraddr = head addrinfos
      sock <- socket (addrFamily serveraddr) Stream defaultProtocol
      connect sock (addrAddress serveraddr)
      val <- getLoop BS.empty sock
      sClose sock
      return val
  where
    getLoop bs conn = do
      msg <- recv conn 2048
      if (BS.null msg) 
        then return bs
        else getLoop (BS.append bs msg) conn

getAsyncBuffer :: ChildLocks -> ServerPort -> MVar BS.ByteString -> IO ()
getAsyncBuffer children serverport ret = do
  lock <- newChildLock children
  _ <- forkIO $ do
    val <- getBuffer serverport
    modifyMVar_ ret (\_ -> return val)
         `finally` endChildLock lock
  return ()

getBufferOuputs :: ChildLocks -> ServerPort -> [PortNumber] 
                   -> MVar [MVar BS.ByteString] -> IO ()
getBufferOuputs children server ports rets = do
  forM_ ports $ \p -> do
    ret <- newMVar BS.empty
    retvals <- takeMVar rets
    putMVar rets (ret:retvals)
    getAsyncBuffer children (newServerPort server p) ret
    
-- -----------------------------------------------------------------------------
-- | Run a Skema Program with a list of input Buffers. Returns the output
-- Buffers.
runBuffers :: [BS.ByteString] -> ServerPort -> ProgramFlow 
              -> IO (Maybe [BS.ByteString])
runBuffers xs server pf = do
  rsend <- sendSkemaProgram server pf
  case rsend of
    Left _ -> return Nothing
    Right pid -> do
      rcreate <- createSkemaRun server $ RPProgramRun (skemaProgramID pid) []
      case rcreate of
        Left _ -> return Nothing
        Right ios -> do
          let iports = map snd $ inPorts ios
              oports = map snd $ outPorts ios
          children <- newChildLocks
          sends <- newMVar []
          rets <- newMVar []
          
          sendBufferInputs children server iports xs sends
          getBufferOuputs children server oports rets
        
          waitForChildren children $ return ()
          
          sendsvals <- withMVar sends (mapM readMVar)
        
          if (all id $ sendsvals)
            then withMVar rets (mapM readMVar) >>= return . Just
            else return Nothing

-- -----------------------------------------------------------------------------
-- | Run a Skema Program with a list of input Buffers and a list of const
-- buffers. Returns the output Buffers.
runBuffersCB :: [BS.ByteString] -> [(PFNodePoint, BS.ByteString)] -> ServerPort 
                -> ProgramFlow -> IO (Maybe [BS.ByteString])
runBuffersCB xs cbuffs server pf = do
  rsend <- sendSkemaProgram server pf
  case rsend of
    Left _ -> return Nothing
    Right pid -> do
      rcreate <- createSkemaRun server $ RPProgramRun (skemaProgramID pid) cbuffs
      case rcreate of
        Left _ -> return Nothing
        Right ios -> do
          let iports = map snd $ inPorts ios
              oports = map snd $ outPorts ios
          children <- newChildLocks
          sends <- newMVar []
          rets <- newMVar []
          
          sendBufferInputs children server iports xs sends
          getBufferOuputs children server oports rets
        
          waitForChildren children $ return ()
          
          sendsvals <- withMVar sends (mapM readMVar)
        
          if (all id $ sendsvals)
            then withMVar rets (mapM readMVar) >>= return . Just
            else return Nothing

-- -----------------------------------------------------------------------------
