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
import Test.HUnit.Utils
import Text.ParserCombinators.Parsec

keysToMap :: Ord k => [k] -> Map.Map k ()
keysToMap = foldl (\map k -> Map.insert k () map) Map.empty

emptymap :: (Eq k, Ord k, Show v) => Map.Map k v
emptymap = Map.empty

q = qc2hu 250
qverbose = qc2huVerbose 250


{- | Test a parser, forcing it to apply to all input. -}
p parser input = 
    case p' parser input of
      Left _ -> Nothing
      Right y -> Just y

p' parser input =
    case parse parseTest "(none)" input of
      Left x -> Left (show x)
      Right y -> Right y
    where parseTest = do r <- parser
                         eof
                         return r

