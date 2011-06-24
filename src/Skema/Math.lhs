%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
module Skema.Math( deg2rad, rad2deg ) where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
-- | 'deg2rad' converts an arc measure from degree units to radian units
deg2rad :: (Floating a) => a -> a
deg2rad d = d * (pi / 180)
\end{code}

\begin{code}
-- | 'deg2rad' converts an arc measure from radian units to degree units
rad2deg :: (Floating a) => a -> a
rad2deg d = d * (180 / pi)
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
