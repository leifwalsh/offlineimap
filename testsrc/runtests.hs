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
           
prop_addFromMaster :: SyncCollection Int Float -> Result
prop_addFromMaster inp =
    let (resMaster, resChild) = syncBiDir inp emptymap emptymap
        expectedResChild = sort . map (\(k, v) -> CopyItem k v) . Map.toList $ inp
        in ([], expectedResChild) @=? 
           (resMaster, sort resChild)

-- FIXME: prop_addFromChild

prop_allChangesToChild :: SyncCollection Int Float -> SyncCollection Int Float -> Result
prop_allChangesToChild master child =
    let (resMaster, resChild) = syncBiDir master child child
        expectedResChild = sort $
            (map (\(k, v) -> CopyItem k v) . Map.toList . Map.difference master $ child) ++
            (map DeleteItem . Map.keys . Map.difference child $ master) ++
            (map (\(k, v) -> ModifyContent k v) changeList)
        changeList = foldl changefunc [] (Map.toList child)
        changefunc accum (k, v) =
            case Map.lookup k master of
              Nothing -> accum
              Just x -> if x /= v
                        then (k, x) : accum
                        else accum
        in ([], expectedResChild) @=?
           (resMaster, sort resChild)

prop_allChangesToMaster :: SyncCollection Int Float -> SyncCollection Int Float -> Result
prop_allChangesToMaster master child =
    let (resMaster, resChild) = syncBiDir master child master
        expectedResMaster = sort $
            (map (\(k, v) -> CopyItem k v) . Map.toList . Map.difference child $ master) ++
            (map DeleteItem . Map.keys . Map.difference master $ child) ++
            (map (\(k, v) -> ModifyContent k v) changeList)
        changeList = foldl changefunc [] (Map.toList child)
        changefunc accum (k, v) =
            case Map.lookup k master of
              Nothing -> accum
              Just x -> if x /= v
                        then (k, v) : accum
                        else accum
        in (expectedResMaster, []) @=?
           (sort resMaster, resChild)

prop_allChanges :: SyncCollection Int Float -> SyncCollection Int Float -> SyncCollection Int Float -> Result
prop_allChanges master child lastchild =
    let (resMaster, resChild) = syncBiDir master child lastchild
        expectedResMaster = sort $
            (map (\(k, v) -> CopyItem k v) . Map.toList . Map.difference child $ Map.union master lastchild) ++
                                                                                            (map DeleteItem . Map.keys . Map.intersection master $ Map.difference lastchild child) ++
            (map (\(k, v) -> ModifyContent k v) masterChanges)

        expectedResChild = sort $
            (map (\(k, v) -> CopyItem k v) . Map.toList . Map.difference master $ Map.union child lastchild) ++
                                                                                            (map DeleteItem . Map.keys . Map.intersection child $ Map.difference lastchild master) ++
            (map (\(k, v) -> ModifyContent k v) childChanges)

        childChanges = foldl (changefunc True) [] (Map.toList child)
        masterChanges = foldl (changefunc False) [] (Map.toList child)
        changefunc useMaster accum (k, v) =
            case Map.lookup k master of
              Nothing -> accum
              Just x -> if x /= v
                        then if useMaster
                             then (k, x) : accum
                             else (k, v) : accum
                        else accum
    in (expectedResMaster, expectedResChild) @=?
       (sort resMaster, sort resChild)

{- | Basic validation that unaryApplyChanges works -}
prop_unaryApplyChanges :: SyncCollection Int Float -> [(Bool, Int, Float)] -> Result
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
prop_unaryApplyChangesId :: SyncCollection Int Float -> SyncCollection Int Float -> Result
prop_unaryApplyChangesId master child =
    let (resMaster, resChild) = syncBiDir master child child
        newMaster = unaryApplyChanges master resMaster
        newChild = unaryApplyChanges child resChild
        newMasterKeys = sort . Map.keys $ newMaster
        newChildKeys = sort . Map.keys $ newChild
        in (True, sort (Map.keys master), sort (Map.keys master)) @=?
           (newMasterKeys == newChildKeys, newMasterKeys, newChildKeys)

prop_unaryApplyChanges3 :: SyncCollection Int Float -> SyncCollection Int Float -> SyncCollection Int Float -> Result
prop_unaryApplyChanges3 master child lastChild =
    let (resMaster, resChild) = syncBiDir master child lastChild
        newMaster = unaryApplyChanges master resMaster
        newChild = unaryApplyChanges child resChild
    in newMaster @=? newChild

prop_diffCollection :: SyncCollection Int Float -> SyncCollection Int Float -> Result
prop_diffCollection coll1 coll2 = 
    let commands = diffCollection coll1 coll2
        newcoll2 = unaryApplyChanges coll1 commands
        in coll2 @=? newcoll2

allt = [qctest "Empty" prop_empty,
        qctest "Del all from child" prop_delAllFromChild,
        qctest "Del all from master" prop_delAllFromMaster,
        qctest "Add from master" prop_addFromMaster,
        qctest "All changes to child" prop_allChangesToChild,
        qctest "All changes to master" prop_allChangesToMaster,
        qctest "All changes" prop_allChanges,
        qctest "unaryApplyChanges" prop_unaryApplyChanges,
        qctest "unaryApplyChangesId" prop_unaryApplyChangesId,
        qctest "unaryApplyChanges3" prop_unaryApplyChanges3,
        qctest "diffCollection" prop_diffCollection
       ]

testh = HU.runTestTT $ HU.TestList allt
testv = runVerbTestText (HU.putTextToHandle stderr True) $ HU.TestList allt

        
main = 
    do testv
       return ()

