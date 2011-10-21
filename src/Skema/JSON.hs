-- -----------------------------------------------------------------------------
-- This file is part of Skema-Common.

-- Skema-Common is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- Skema-Common is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.

-- You should have received a copy of the GNU Affero General Public License
-- along with Skema-Common.  If not, see <http://www.gnu.org/licenses/>.
-- -----------------------------------------------------------------------------
-- | JSON utility functions.
module Skema.JSON( prettyJSON ) where

-- -----------------------------------------------------------------------------
-- | Generate a pretty string from a JSON string, putting line-breaks.
prettyJSON :: String -- ^ original JSON string
              -> String -- ^ return prettified string
prettyJSON buff = prettyJSON' buff 0

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
removeInnerString ('\\':'\\':xs) = ("\\\\"++ys,zs)
  where
    (ys,zs) = removeInnerString xs
removeInnerString ('\\':'"':xs) = ("\\\"" ++ ys,zs)
  where
    (ys,zs) = removeInnerString xs
removeInnerString ('"':xs) = ("\"",xs)
removeInnerString (x:xs) = (x:ys,zs)
  where
    (ys,zs) = removeInnerString xs

lvlTabs :: Int -> String
lvlTabs n = replicate (4*n) ' '

-- -----------------------------------------------------------------------------
