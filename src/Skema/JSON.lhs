%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of Skema-Common.

% Skema-Common is free software: you can redistribute it and/or modify
% it under the terms of the GNU Affero General Public License as
% published by the Free Software Foundation, either version 3 of the
% License, or (at your option) any later version.

% Skema-Common is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Affero General Public License for more details.

% You should have received a copy of the GNU Affero General Public License
% along with Skema-Common.  If not, see <http://www.gnu.org/licenses/>.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
-- | JSON utility functions.
module Skema.JSON( prettyJSON, jsonLookup, smapToObj, objToSmap ) where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Text.JSON( JSON(..), Result(..), JSValue(..), makeObj, fromJSObject )
import qualified Data.Map as M( Map, assocs, fromList )
import Control.Arrow( second )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
-- | Looks up a key in an association list during JSON decode.
jsonLookup :: String -- ^ key
              -> [(String, a)] -- ^ association list
              -> Result a -- ^ return the decoded value
jsonLookup a as = maybe (fail $ "No element: " ++ a) return (lookup a as)
\end{code}

\begin{code}
-- | Convert a `Map` String Value in a JSON object 
smapToObj :: JSON a => M.Map String a -- ^ Map of values
             -> JSValue -- ^ return the JSON object
smapToObj = makeObj . map (second showJSON) . M.assocs
\end{code}

\begin{code}
extractResult :: (a,Result b) -> Result (a,b)
extractResult (_, Error e) = Error e
extractResult (a, Ok b) = Ok (a,b)
\end{code}

\begin{code}
-- | Decode a JSON object as a `Map` of `String` keys
objToSmap :: JSON a => JSValue -- ^ JSON object to decode
             -> Result (M.Map String a) -- ^ return the decoded Map
objToSmap (JSObject obj) = do
  vals <- sequence . map (extractResult . second readJSON) $ fromJSObject obj
  return $ M.fromList vals
objToSmap _ = fail "obj to smap"
\end{code}

\begin{code}
-- | Generate a pretty string from a JSON string, putting line-breaks.
prettyJSON :: String -- ^ original JSON string
              -> String -- ^ return prettified string
prettyJSON buff = prettyJSON' buff 0
\end{code}

\begin{code}
prettyJSON' :: String -> Int -> String
prettyJSON' [] _ = []
prettyJSON' ('{':xs) lvl = "{\n" ++ lvlTabs (lvl+1) ++ prettyJSON' xs (lvl+1)
prettyJSON' ('}':xs) lvl = '}' : prettyJSON' xs (lvl-1)
prettyJSON' (',':xs) lvl = ",\n" ++ lvlTabs lvl ++ prettyJSON' xs lvl
prettyJSON' ('[':xs) lvl = '[' : prettyJSON' xs (lvl+1)
prettyJSON' (']':xs) lvl = ']' : prettyJSON' xs (lvl-1)
prettyJSON' ('"':xs) lvl = ('"':ys) ++ prettyJSON' zs lvl
  where
    (ys,zs) = removeInnerString xs
prettyJSON' (x:xs) lvl = x : prettyJSON' xs lvl

removeInnerString :: String -> (String,String)
removeInnerString [] = ([],[])
removeInnerString ('\\':'"':xs) = ("\\\"" ++ ys,zs)
  where
    (ys,zs) = removeInnerString xs
removeInnerString ('"':xs) = ("\"",xs)
removeInnerString (x:xs) = (x:ys,zs)
  where
    (ys,zs) = removeInnerString xs

lvlTabs :: Int -> String
lvlTabs n = replicate (4*n) ' '
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
