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

prop_identity :: String -> Bool
prop_identity f = runStringConnection f (\_ -> return ()) == ((), (f, []))

prop_linesidentity :: String -> Bool
prop_linesidentity f =
    runLinesConnection [f] (\_ -> return ()) == ((), (f ++ "\r\n", []))

prop_lineslistidentity :: [String] -> Bool
prop_lineslistidentity f =
    runLinesConnection f (\_ -> return ()) == ((), (expected, []))
    where expected = 
              case f of
                [] -> []
                _ -> (intercalate "\r\n" f) ++ "\r\n"

q :: Testable a => String -> a -> HU.Test
q = qccheck (defaultConfig {configMaxTest = 250})

allt = [q "Identity" prop_identity,
        q "Lines identity" prop_linesidentity,
        q "Lines list identity" prop_lineslistidentity
       ]
