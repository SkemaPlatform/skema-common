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
-- | Mathematical Utilities for Skema programs
module Skema.Math( deg2rad, rad2deg ) where

-- -----------------------------------------------------------------------------
-- | 'deg2rad' converts an arc measure from degree units to radian units
deg2rad :: (Floating a) => a -> a
deg2rad d = d * (pi / 180)

-- | 'deg2rad' converts an arc measure from radian units to degree units
rad2deg :: (Floating a) => a -> a
rad2deg d = d * (180 / pi)

-- -----------------------------------------------------------------------------

