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

module Main where
import Test.QuickCheck
import Test.QuickCheck.Batch
import qualified Test.HUnit as HU
import Test.HUnit.Utils
import qualified Data.Map as Map

import Data.Syncable

prop_empty =
    syncThem emptymap emptymap emptymap == ([], [], [])

keysToMap :: Ord k => [k] -> Map.Map k ()
keysToMap = foldl (\map k -> Map.insert k () map) Map.empty

emptymap :: Map.Map Integer ()
emptymap = Map.empty

allt = [qctest "Empty" prop_empty]

testh = HU.runTestTT $ HU.TestList allt
         
main = 
    do testh
       return ()
