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
module Skema.JSON( prettyJSON, jsonLookup ) where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Text.JSON( Result )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
jsonLookup :: String -> [(String, a)] -> Result a
jsonLookup a as = maybe (fail $ "No element: " ++ a) return (lookup a as)
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
