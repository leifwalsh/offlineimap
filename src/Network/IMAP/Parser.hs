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

module Network.IMAP.Parser where
import Text.ParserCombinators.Parsec
import Network.IMAP.Types
import Text.Regex.Posix
import Data.Int
import Data.List

import Network.IMAP.Parser.Prim

{- | Read a full response from the server. -}
readFullResponse :: Monad m => 
    IMAPConnection m ->         -- ^ The connection to the server
    m IMAPString
readFullResponse conn =
    accumLines []
    where accumLines accum = 
              do line <- getFullLine [] conn
                 if "* " `isPrefixOf` line
                    then accumLines (accum ++ line ++ "\r\n")
                    else return (accum ++ line ++ "\r\n")

{- | Read a full line from the server, handling any continuation stuff.

If a {x}\r\n occurs, then that string (including the \r\n) will occur
literally in the result, followed by the literal read, and the rest of the
data.
 -}

getFullLine :: Monad m => 
               IMAPString ->    -- ^ The accumulator (empty for first call)
               IMAPConnection m -> -- ^ IMAP connection
               m IMAPString        -- ^ Result

getFullLine accum conn =
    do input <- readLine conn
       case checkContinuation input of
         Nothing -> return (accum ++ input)
         Just (size) -> 
             do literal <- readBytes conn size
                getFullLine (accum ++ input ++ "\r\n" ++ literal) conn
    where checkContinuation :: String -> Maybe Int64
          checkContinuation i =
              case i =~ "\\{([0-9]+)\\}$" :: (String, String, String, [String]) of
                (_, _, _, [x]) -> Just (read x)
                _ -> Nothing

----------------------------------------------------------------------
-- Response parsing
----------------------------------------------------------------------

{- | Returns Left for a "BYE" response, or Right if we are ready to
proceed with auth (or preauth). -}
greeting :: IMAPParser (Either RespText (AuthReady, RespText))
greeting =
    do string "* "
       (respCondBye >>= return . Left) <|>
          (respCondAuth >>= return . Right)

data AuthReady = AUTHOK | AUTHPREAUTH
          deriving (Eq, Read, Show)

data RespText = RespText {respTextCode :: Maybe String,
                           respTextMsg :: String}
                 deriving (Eq, Read, Show)

respCondAuth :: IMAPParser (AuthReady, RespText)
respCondAuth =
    do s <- (string "OK" >> return AUTHOK) <|>
            (string "PREAUTH" >> return AUTHPREAUTH)
       sp
       t <- respText
       return (s, t)

respCondBye :: IMAPParser RespText
respCondBye =
    do string "BYE "
       respText

-- Less strict than mandated in RFC3501 formal syntax
respText :: IMAPParser RespText
respText =
    do code <- optionMaybe respTextCode
       t <- text
       return $ RespText code t
    where respTextCode =
              do char '['
                 a <- atom
                 b <- option "" (sp >> respTextCodeText)
                 char ']'
                 sp
                 return (a ++ " " ++ b)
          respTextCodeText = many1 (noneOf (']' : crlf))
                 