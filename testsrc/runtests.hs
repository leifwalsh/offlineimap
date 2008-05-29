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
import Data.List
import Data.Maybe(catMaybes)
import System.IO(stderr)
import Data.Word

import Data.Syncable
import TestInfrastructure

prop_empty :: Bool
prop_empty =
    syncBiDir (emptymap::Map.Map Int ()) emptymap emptymap == ([], []) -- ([DeleteItem 5], [], [])

prop_delAllFromChild :: SyncCollection Int () -> Result
prop_delAllFromChild inp =
    let (resMaster, resChild) = syncBiDir emptymap inp inp
        expectedResChild = sort . map DeleteItem . Map.keys $ inp
        in ([], expectedResChild) @=? 
           (resMaster, sort resChild)
           
prop_delAllFromMaster :: SyncCollection Int () -> Result
prop_delAllFromMaster inp =
    let (resMaster, resChild) = syncBiDir inp emptymap inp
        expectedResMaster = sort . map DeleteItem . Map.keys $ inp
        in (expectedResMaster, []) @=? 
           (sort resMaster, resChild)
           
prop_addFromMaster :: SyncCollection Int Word8 -> Result
prop_addFromMaster inp =
    let (resMaster, resChild) = syncBiDir inp emptymap emptymap
        expectedResChild = sort . map (\(k, v) -> CopyItem k v) . Map.toList $ inp
        in ([], expectedResChild) @=? 
           (resMaster, sort resChild)

-- FIXME: prop_addFromChild

prop_allChangesToChild :: SyncCollection Int Word8 -> SyncCollection Int Word8 -> Result
prop_allChangesToChild master child =
    let (resMaster, resChild) = syncBiDir master child child
        expectedResChild = sort $
            (map (\(k, v) -> CopyItem k v) . Map.toList . Map.difference master $ child) ++
            (map DeleteItem . Map.keys . Map.difference child $ master) ++
            (map (pairToFunc ModifyContent) changeList)
        changeList = foldl changefunc [] (Map.toList child)
        changefunc accum (k, v) =
            case Map.lookup k master of
              Nothing -> accum
              Just x -> if x /= v
                        then (k, x) : accum
                        else accum
        in ([], expectedResChild) @=?
           (sort resMaster, sort resChild)

prop_allChangesToMaster :: SyncCollection Int Word8 -> SyncCollection Int Word8 -> Result
prop_allChangesToMaster master child =
    let (resMaster, resChild) = syncBiDir master child master
        expectedResMaster = sort $
            (map (pairToFunc CopyItem) . Map.toList . Map.difference child $ master) ++
            (map DeleteItem . Map.keys . Map.difference master $ child) ++
            (map (pairToFunc ModifyContent) changeList)
        changeList = foldl changefunc [] (Map.toList child)
        changefunc accum (k, v) =
            case Map.lookup k master of
              Nothing -> accum
              Just x -> if x /= v
                        then (k, v) : accum
                        else accum
        in (expectedResMaster, []) @=?
           (sort resMaster, resChild)

-- FIXME: test findModified

prop_allChanges :: SyncCollection Int Word8 -> SyncCollection Int Word8 -> SyncCollection Int Word8 -> Result
prop_allChanges master child lastchild =
    let (resMaster, resChild) = syncBiDir master child lastchild

        masterMods = catMaybes . map procKV $ (Map.toList master)
            where procKV (k, m) =
                      case (Map.lookup k child, Map.lookup k lastchild) of
                        (Just c, Just lc) -> 
                            if c == lc -- child didn't change
                               then Nothing
                               else if c == m -- child and master changed
                                    then Nothing
                                    else Just (k, c) -- child changed, master didn't
                        (Nothing, Just lc) -> Nothing -- deleted on child
                        (Just c, Nothing) -> -- New on both c and m
                            if c == m        -- Added the same
                               then Nothing
                               else Just (k, c) -- Added but differ
                        (Nothing, Nothing) -> Nothing -- New to master only

        childMods = catMaybes . map procKV $ (Map.toList child)
            where procKV (k, c) =
                      case (Map.lookup k master, Map.lookup k lastchild) of
                        (Just m, Just lc) -> 
                            if lc == c 
                               then if c == m
                                       then Nothing
                                       else Just (k, m)
                               else Nothing
                        (Nothing, Just lc) ->        -- deleted; nothing to see here
                                              Nothing
                        (Just m, Nothing) -> -- New on both; child takes precedence
                           Nothing
                        (Nothing, Nothing) -> Nothing -- New to child only

        expectedResMaster = sort $
            (map (pairToFunc CopyItem) . Map.toList . Map.difference child $ Map.union master lastchild) ++
                                                                                            (map DeleteItem . Map.keys . Map.intersection master $ Map.difference lastchild child) ++
            (map (pairToFunc ModifyContent) masterMods)

        expectedResChild = sort $
            (map (pairToFunc CopyItem) . Map.toList . Map.difference master $ Map.union child lastchild) ++
                                                                                            (map DeleteItem . Map.keys . Map.intersection child $ Map.difference lastchild master) ++
            (map (pairToFunc ModifyContent) childMods)

    in (expectedResMaster, expectedResChild) @=?
       (sort resMaster, sort resChild)

{- | Basic validation that unaryApplyChanges works -}
prop_unaryApplyChanges :: SyncCollection Int Word8 -> [(Bool, Int, Word8)] -> Result
prop_unaryApplyChanges collection randcommands =
    let -- We use nubBy to make sure we don't get input that has reference
        -- to the same key more than once.  We then convert True/False to
        -- commands.
        commands = map toCommand . nubBy (\(x1, y1, z1) (x2, y2, z2) -> y1 == y2) $ randcommands
        toCommand (True, x, v) = CopyItem x v
        toCommand (False, x, _) = DeleteItem x

        addedItems = catMaybes . map (\x -> case x of CopyItem y v -> Just (y, v); _ -> Nothing) $ commands
        deletedKeys = catMaybes . map (\x -> case x of DeleteItem y -> Just y; _ -> Nothing) $ commands
        
        collection' = foldl (flip Map.delete) collection deletedKeys
        expectedCollection = 
            Map.union collection' (Map.fromList addedItems)
        in (sort . Map.keys $ expectedCollection) @=?
           (sort . Map.keys $ unaryApplyChanges collection commands)

{- | Should validate both that unaryApplyChanges works, and that it is
an identify -}
prop_unaryApplyChangesId :: SyncCollection Int Word8 -> SyncCollection Int Word8 -> Result
prop_unaryApplyChangesId master child =
    let (resMaster, resChild) = syncBiDir master child child
        newMaster = unaryApplyChanges master resMaster
        newChild = unaryApplyChanges child resChild
        newMasterKeys = sort . Map.keys $ newMaster
        newChildKeys = sort . Map.keys $ newChild
        in (True, sort (Map.keys master), sort (Map.keys master)) @=?
           (newMasterKeys == newChildKeys, newMasterKeys, newChildKeys)

prop_unaryApplyChanges3 :: SyncCollection Int Word8 -> SyncCollection Int Word8 -> SyncCollection Int Word8 -> Result
prop_unaryApplyChanges3 master child lastChild =
    let (resMaster, resChild) = syncBiDir master child lastChild
        newMaster = unaryApplyChanges master resMaster
        newChild = unaryApplyChanges child resChild
    in newMaster @=? newChild

prop_diffCollection :: SyncCollection Int Word8 -> SyncCollection Int Word8 -> Result
prop_diffCollection coll1 coll2 = 
    let commands = diffCollection coll1 coll2
        newcoll2 = unaryApplyChanges coll1 commands
        in coll2 @=? newcoll2

q :: Testable a => String -> a -> HU.Test
q = qccheck (defaultConfig {configMaxTest = 250})

allt = [q "Empty" prop_empty,
        q "Del all from child" prop_delAllFromChild,
        q "Del all from master" prop_delAllFromMaster,
        q "Add from master" prop_addFromMaster,
        q "All changes to child" prop_allChangesToChild,
        q "All changes to master" prop_allChangesToMaster,
        q "All changes" prop_allChanges,
        q "unaryApplyChanges" prop_unaryApplyChanges,
        q "unaryApplyChangesId" prop_unaryApplyChangesId,
        q "unaryApplyChanges3" prop_unaryApplyChanges3,
        q "diffCollection" prop_diffCollection
       ]

testh = HU.runTestTT $ HU.TestList allt
testv = runVerbTestText (HU.putTextToHandle stderr True) $ HU.TestList allt

        
main = 
    do testv
       return ()

