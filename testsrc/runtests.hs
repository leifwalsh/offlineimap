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
import System.IO
import Text.Printf
import Data.List

import Data.Syncable

prop_empty :: Bool
prop_empty =
    syncThem emptymap emptymap emptymap == ([], [], []) -- ([DeleteItem 5], [], [])

prop_delAllFromChild :: SyncCollection Int -> Bool
prop_delAllFromChild inp =
    let (resMaster, resChild, resState) = syncThem emptymap inp inp
        expectedResChild = sort . map DeleteItem . Map.keys $ inp
        in resMaster == [] &&
           (sort resChild == expectedResChild) &&
           (sort resState == expectedResChild)

prop_addFromMaster :: SyncCollection Int -> Bool
prop_addFromMaster inp =
    let (resMaster, resChild, resState) = syncThem inp emptymap emptymap
        expectedResChild = sort . map CopyItem . Map.keys $ inp
        in (resMaster == []) &&
           (sort resChild == expectedResChild) &&
           (sort resState == expectedResChild)

keysToMap :: Ord k => [k] -> Map.Map k ()
keysToMap = foldl (\map k -> Map.insert k () map) Map.empty

emptymap :: Map.Map Int ()
emptymap = Map.empty

allt = [("Empty", prop_empty),
        ("Del all from child", prop_delAllFromChild)]
alltHU = map (\(str, prop) -> qctest str prop) allt

testh = HU.runTestTT $ HU.TestList alltHU
testv = runVerbTestText (HU.putTextToHandle stderr True) $ HU.TestList alltHU

testq = runTests "Test Stuff" defOpt (map (run . snd) allt)

instance (Arbitrary k, Eq k, Ord k) => Arbitrary (Map.Map k ()) where
    arbitrary = 
        do items <- arbitrary
           return $ keysToMap items
    coarbitrary = coarbitrary . Map.keys

-- Modified from HUnit
runVerbTestText :: HU.PutText st -> HU.Test -> IO (HU.Counts, st)
runVerbTestText (HU.PutText put us) t = do
  (counts, us') <- HU.performTest reportStart reportError reportFailure us t
  us'' <- put (HU.showCounts counts) True us'
  return (counts, us'')
 where
  reportStart ss us = do hPrintf stderr "\rTesting %-68s\n" (HU.showPath (HU.path ss))
                         put (HU.showCounts (HU.counts ss)) False us
  reportError   = reportProblem "Error:"   "Error in:   "
  reportFailure = reportProblem "Failure:" "Failure in: "
  reportProblem p0 p1 msg ss us = put line True us
   where line  = "### " ++ kind ++ path' ++ '\n' : msg
         kind  = if null path' then p0 else p1
         path' = HU.showPath (HU.path ss)

         
main = 
    do testv
       return ()

