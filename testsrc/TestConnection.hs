{-
Copyright (C) 2002-2008 John Goerzen <jgoerzen@complete.org>

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
module TestConnection where
import qualified Data.Map as Map
import Data.List
import qualified Test.HUnit as HU
import Test.HUnit.Utils
import Data.Word
import Test.QuickCheck
import TestInfrastructure

import Network.IMAP.Connection
import Network.IMAP.Types

prop_identity :: String -> Bool
prop_identity f = runStringConnection f (\_ -> return ()) == Right ((), (f, []))

prop_linesidentity :: String -> Bool
prop_linesidentity f =
    runLinesConnection [f] (\_ -> return ()) == Right ((), (f ++ "\r\n", []))

prop_lineslistidentity :: [String] -> Property
prop_lineslistidentity f =
    and (map (notElem '\r') f)  ==> 
        runLinesConnection f (\_ -> return ()) @?= Right ((), (expected, []))
    where expected = expectedString f

expectedString f =
              case f of
                [] -> []
                _ -> (intercalate "\r\n" f) ++ "\r\n"

noCR :: [String] -> Bool
noCR s = and (map (\e -> notElem '\r' e && notElem '\NUL' e) s)

prop_readLine :: [String] -> Property
prop_readLine s =
    noCR s ==> 
        runLinesConnection s readLine @?=
            if null s
               then Left "EOF in input in readLine"
               else Right (head s, (expectedString (tail s), []))

prop_readBytes :: String -> Int -> Result
prop_readBytes s l =
      runStringConnection s (\c -> readBytes c (fromIntegral l)) @?=
          if l < 0
             then Left "readBytes: negative count"
             else case compare l (length s) of
                    EQ -> Right (take l s, (drop l s, []))
                    LT -> Right (take l s, (drop l s, []))
                    GT -> Left "EOF in input in readBytes"

allt = [q "Identity" prop_identity,
        q "Lines identity" prop_linesidentity,
        q "Lines list identity" prop_lineslistidentity,
        q "readline" prop_readLine,
        q "readBytes" prop_readBytes
       ]
