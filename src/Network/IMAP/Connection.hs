{- offlineimap component
Copyright (C) 2008 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module Network.IMAP.Connection where
import Network.IMAP.Types
import Control.Monad.State
import Data.List.Utils(spanList)
import Data.List(genericSplitAt, genericLength, intercalate)

type IMAPState = State (IMAPString, IMAPString)

{- | Set up an IMAPConnection that runs in the State monad.

The state is (bufferToClient, bufferFromClient)

Remember that EOL in IMAP protocols is \r\n!

closeConnection is ignored with this monad. -}
newStringConnection :: IMAPConnection IMAPState
newStringConnection =
    IMAPConnection {readBytes = lreadBytes,
                    readLine = lreadLine,
                    writeBytes = lwriteBytes,
                    closeConn = return ()}
    where 
          lreadBytes count = 
              do (s,sw) <- get
                 if genericLength s < count
                    then fail "EOF in input in readBytes"
                    else do let (r, s') = genericSplitAt count s
                            put (s', sw)
                            return r
          lreadLine =
              do (s, sw) <- get
                 let (line, remainder) = spanList (\x -> "\r\n" /= take 2 x) s
                 case remainder of
                   [] -> fail "EOF in input in readLine"
                   r -> do put (drop 2 r, sw) -- strip off \r\n
                           return line

          lwriteBytes outdata =
              do (s, sw) <- get
                 put (s, sw ++ outdata)

{- | Runs a State monad with a String connection.  Returns
(retval, remainingBufferToClient, bufferFromClient) -}
runStringConnection ::
       IMAPString               -- ^ Buffer to send to clients
    -> (IMAPConnection IMAPState -> IMAPState a) -- ^ Function to run
    -> (a, (String, String))    -- ^ Results: func result, buffer status
runStringConnection sbuf func =
    runState (func newStringConnection) (sbuf::String, []::String)

{- | Runs a State monad with a String connection, initializing it with 
the passed lines.  -}
runLinesConnection ::
       [IMAPString]             -- ^ Buffer to send to clients
    -> (IMAPConnection IMAPState -> IMAPState a) -- ^ Function to run
    -> (a, (String, String))    -- ^ Results: func result, buffer status
runLinesConnection sbuf func 
    | sbuf == [] = 
        -- For the empty input, no \r\n after.
        runStringConnection [] func
    | otherwise = 
        -- Put \r\n between the lines, and also after the last one.
        runStringConnection (intercalate "\r\n" sbuf ++ "\r\n") func