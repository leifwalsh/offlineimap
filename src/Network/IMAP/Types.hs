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

module Network.IMAP.Types where
import Data.Int

-- | The fundamental type of low-level communication
type IMAPString = String

-- | The low-level IMAP connection type.
data (Monad m) => IMAPConnection m = IMAPConnection {
      readBytes :: Int64 -> m IMAPString, -- ^ Read exactly x bytes
      readLine :: m IMAPString,           -- ^ Read next line, stripping off trailing CRLF
      writeBytes :: IMAPString -> m (),   -- ^ Write text to server
      closeConn :: m ()                   -- ^ Close IMAP connection
    }

