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
jsonLookup :: String -> [(String, a)] -> Result a
jsonLookup a as = maybe (fail $ "No element: " ++ a) return (lookup a as)
\end{code}

\begin{code}
smapToObj :: JSON a => M.Map String a -> JSValue
smapToObj = makeObj . map (second showJSON) . M.assocs
\end{code}

\begin{code}
extractResult :: (a,Result b) -> Result (a,b)
extractResult (_, Error e) = Error e
extractResult (a, Ok b) = Ok (a,b)
\end{code}

\begin{code}
objToSmap :: JSON a => JSValue -> Result (M.Map String a)
objToSmap (JSObject obj) = do
  vals <- sequence . map (extractResult . second readJSON) $ fromJSObject obj
  return $ M.fromList vals
objToSmap _ = fail "2"
\end{code}

\begin{code}
prettyJSON :: String -> String
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
prettyJSON' (x:xs) lvl = x : prettyJSON' xs lvl

lvlTabs :: Int -> String
lvlTabs n = replicate (4*n) ' '
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
