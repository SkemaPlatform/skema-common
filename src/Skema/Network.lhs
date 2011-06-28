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
{-# LANGUAGE MultiParamTypeClasses #-}
module Skema.Network( postMultipartData, postFormUrlEncoded ) where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Data.List( intercalate )
import Data.URLEncoded( export, importList )
import Network.HTTP
  ( Request(..), RequestMethod(POST), HeaderName(..), Header(..) )
import Network.URI
  (  parseURI )
import System.Random( randomRIO )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
postMultipartData :: String -> String -> IO (Request String)
postMultipartData uri buf = do
  case parseURI uri of
    Nothing -> error "invalid uri"
    Just u -> do
      (enc, body) <- encodeMultipartFormData [("data",buf)]
      return Request
            { rqMethod=POST
            , rqHeaders=[ Header HdrContentType enc
                        , Header HdrContentLength (show . length $ body) ]
            , rqBody=body
            , rqURI=u }
\end{code}

\begin{code}
genBoundary :: IO String
genBoundary = do
  rand <- randomRIO (100000000000 :: Integer, 999999999999)
  return $ "--------------------" ++ show rand
\end{code}

\begin{code}
encodeMultipartFormData :: [(String,String)] -> IO (String,String)
encodeMultipartFormData fields = do
  bnd <- genBoundary
  let body = (concatMap (encodeField bnd) fields) ++ ["--"++bnd++"--",""]
  let contentType = "multipart/form-data; boundary=" ++ bnd
  return (contentType, intercalate "\r\n" body)
\end{code}

\begin{code}
encodeField :: String -> (String,String) -> [String]
encodeField bnd (key,val) = [ "--" ++ bnd
                            , "Content-Disposition: form-data; name=" ++ key
                            , ""
                            , val ]
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
postFormUrlEncoded :: String -> [(String,String)] -> IO (Request String)
postFormUrlEncoded uri pairs = do
  let form = export . importList $ pairs
  case parseURI uri of
    Nothing -> error $ "invalid uri "++ uri
    Just u -> do
      return Request {
        rqMethod=POST,
        rqHeaders=[ Header HdrContentType "application/x-www-form-urlencoded", 
                    Header HdrContentLength (show . length $ form) ],
        rqBody=form,                  
        rqURI=u }
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
