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
-- | Functions and types to help in the use of concurrent threads
module Skema.Concurrent( 
  -- * Types
  ChildLock, ChildLocks, 
  -- * Functions
  newChildLock, newChildLocks, endChildLock, waitForChildren ) 
       where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Control.Concurrent.MVar
  ( MVar, newMVar, newEmptyMVar, takeMVar, putMVar )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
-- | The ChildLock is a Synchronising variable to test a thread termination
type ChildLock = MVar ()
-- | The ChildLocks save all the children that we want to wait
type ChildLocks = MVar [ChildLock]
\end{code}

\begin{code}
-- | Create a new Child Lock to wait for later
newChildLock :: ChildLocks -- ^ list of children locks we'll save the new one
                -> IO ChildLock -- ^ return the new child lock
newChildLock children = do
  lock <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (lock:childs)
  return lock
\end{code}
                                 
\begin{code}
-- | End a child lock. This should be called for the child thread when done
endChildLock :: ChildLock -- ^ thread lock
                -> IO ()
endChildLock lock = putMVar lock ()
\end{code}
                                 
\begin{code}
-- | Create a new empty list of children to wait for
newChildLocks :: IO ChildLocks -- ^ return the new list
newChildLocks = newMVar []
\end{code}
                                 
\begin{code}
waitForChild :: ChildLock -> IO ()
waitForChild = takeMVar
\end{code}
                                 
\begin{code}
-- | Wait until the children listed call `endChildLock` with its own lock
waitForChildren :: ChildLocks -- ^ list of children locks to wait for
                   -> IO () -- ^ function to call at the end
                   -> IO ()
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
