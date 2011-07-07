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
module Skema.Concurrent( 
  ChildLock, ChildLocks, newChildLock, newChildLocks, endChildLock, 
  waitForChildren ) 
       where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Control.Concurrent.MVar
  ( MVar, newMVar, newEmptyMVar, takeMVar, putMVar )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
type ChildLock = MVar ()
type ChildLocks = MVar [ChildLock]
\end{code}

\begin{code}
newChildLock :: ChildLocks -> IO ChildLock
newChildLock children = do
  lock <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (lock:childs)
  return lock
\end{code}
                                 
\begin{code}
endChildLock :: ChildLock -> IO ()
endChildLock lock = putMVar lock ()
\end{code}
                                 
\begin{code}
newChildLocks :: IO ChildLocks 
newChildLocks = newMVar []
\end{code}
                                 
\begin{code}
waitForChild :: ChildLock -> IO ()
waitForChild = takeMVar
\end{code}
                                 
\begin{code}
waitForChildren :: ChildLocks -> IO () -> IO ()
waitForChildren children end = do
      cs <- takeMVar children
      case cs of
        []   -> end
        m:ms -> do
           putMVar children ms
           waitForChild m
           waitForChildren children end
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
