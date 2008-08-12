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

module TestInfrastructure where
import Test.QuickCheck
import Test.QuickCheck.Batch
import qualified Test.HUnit as HU
import qualified Data.Map as Map
import System.IO
import Text.Printf
import System.Random
import Data.Word

(@=?) :: (Eq a, Show a) => a -> a -> Result
expected @=? actual = 
        Result {ok = Just (expected == actual), 
                arguments = ["Result: expected " ++ show expected ++ ", got " ++ show actual],
                stamp = []}
    
(@?=) :: (Eq a, Show a) => a -> a -> Result
(@?=) = flip (@=?)

keysToMap :: Ord k => [k] -> Map.Map k ()
keysToMap = foldl (\map k -> Map.insert k () map) Map.empty

emptymap :: (Eq k, Ord k, Show v) => Map.Map k v
emptymap = Map.empty

instance (Arbitrary k, Arbitrary v, Eq k, Ord k) => Arbitrary (Map.Map k v) where
    arbitrary = 
        do items <- arbitrary
           return $ Map.fromList items
    coarbitrary = coarbitrary . Map.keys

instance Arbitrary Word8 where
    arbitrary = sized $ \n -> choose (0, min (fromIntegral n) maxBound)
    coarbitrary n = variant (if n >= 0 then 2 * x else 2 * x + 1)
                where x = abs . fromIntegral $ n

instance Random Word8 where
    randomR (a, b) g = (\(x, y) -> (fromInteger x, y)) $
                       randomR (toInteger a, toInteger b) g
    random g = randomR (minBound, maxBound) g

instance Arbitrary Char where
    arbitrary = sized $ \n -> choose (toEnum 0, min (toEnum n) maxBound)
    coarbitrary n = variant (if (fromEnum n) >= 0 then toEnum (2 * x) else toEnum (2 * x + 1))
                where x = (abs . fromEnum $ n)::Int

-- Modified from HUnit
runVerbTestText :: HU.PutText st -> HU.Test -> IO (HU.Counts, st)
runVerbTestText (HU.PutText put us) t = do
  (counts, us') <- HU.performTest reportStart reportError reportFailure us t
  us'' <- put (HU.showCounts counts) True us'
  return (counts, us'')
 where
  reportStart ss us = do hPrintf stdout "\rTesting %-68s\n" (HU.showPath (HU.path ss))
                         put (HU.showCounts (HU.counts ss)) False us
  reportError   = reportProblem "Error:"   "Error in:   "
  reportFailure = reportProblem "Failure:" "Failure in: "
  reportProblem p0 p1 msg ss us = put line True us
   where line  = "### " ++ kind ++ path' ++ '\n' : msg
         kind  = if null path' then p0 else p1
         path' = HU.showPath (HU.path ss)

