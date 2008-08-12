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

module Network.IMAP.Parser.Prim where
import Text.ParserCombinators.Parsec
import Network.IMAP.Types

----------------------------------------------------------------------
-- RFC 2234 (ABNF) declarations
----------------------------------------------------------------------

-- | RFC 2234
ctl :: String
ctl = '\x7f' : ['\x00'..'\x1f'] 

-- | RFC 2234
cr, lf :: Char
cr = '\x0d'
lf = '\x0a'

-- | RFC 2234
crlf :: String
crlf = [cr, lf]

-- | RFC 2234
digit2234 :: String
digit2234 = ['0'..'9']

-- | RFC 2234
hexdig :: String
hexdig = digit2234 ++ ['A'..'F']

-- | RFC 2234
alpha :: String
alpha = ['A'..'Z'] ++ ['a'..'z']

-- | RFC 2234
dquote :: Char
dquote = '"'

-- | RFC 2234
char2234 :: String
char2234 = ['\x01'..'\x7f']

-- | RFC 2234
sp :: Char
sp = ' '

-- | RFC 2234

----------------------------------------------------------------------
-- RFC 3501 primitives
----------------------------------------------------------------------

-- | RFC 3501
atomSpecials :: String
atomSpecials = "(){ " ++ ctl ++ listWildcards ++ quotedSpecials ++ respSpecials

listWildcards :: String
listWildcards = "%*"

quotedSpecials :: String
quotedSpecials = ['\\', dquote]

respSpecials :: String
respSpecials = "]"

----------------------------------------------------------------------
-- Parsec RFC 3501 primitives
----------------------------------------------------------------------

-- | Technically, <any CHAR except atom-specials> but we will 
-- be more permissive.
--
-- FIXME: not stated in RFC, but perhaps CRLF is also excluded?
atomChar = noneOf atomSpecials

atom = many1 atomChar

astringChar = atomChar <|> oneOf respSpecials

astring = many1 astringChar <|> string3501

string3501 = quoted <|> literal

literal = 
    do char '{'
       scount <- many1 digit
       char '}'
       string crlf
       let icount = (read scount)::Int
       count icount anyChar
       
quoted = 
    do char dquote
       strdata <- many quotedChar
       char dquote
       return strdata

quotedChar = 
    noneOf quotedSpecials <|> (do char '\\'
                                  oneOf quotedSpecials
                              )
           