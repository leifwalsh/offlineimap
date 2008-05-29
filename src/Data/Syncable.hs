{- offlineimap component
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

{-
The OfflineIMAP v6 algorithm worked like this:

call remoterepos.syncfoldersto(localrepos, [statusrepos])

for each folder, call 
 syncfolder(remotename, remoterepos, remotefolder, localrepos, statusrepos, quick)
   this:
   sets localfolder = local folder
   adds localfolder to mbnames
   sets statusfolder = status folder
   if localfolder.getuidvalidity() == None, removes anything in statusfolder
   
   statusfolder.cachemessagelist()

   localfolder.cachemessagelist()

   Check UID validity
   Save UID validity

   remotefolder.cachemessagelist()

   if not statusfolder.isnewfolder():
        # Delete local copies of remote messages.  This way,
        # if a message's flag is modified locally but it has been
        # deleted remotely, we'll delete it locally.  Otherwise, we
        # try to modify a deleted message's flags!  This step
        # need only be taken if a statusfolder is present; otherwise,
        # there is no action taken *to* the remote repository.
        remotefolder.syncmessagesto_delete(localfolder, [localfolder,
                                                         statusfolder])
        localfolder.syncmessagesto(statusfolder, [remotefolder, statusfolder])

   # Synchroonize remote changes
   remotefolder.syncmessagesto(localfolder, [localfolder, statusfolder])

   # Make sure the status folder is up-to-date.
   ui.syncingmessages(localrepos, localfolder, statusrepos, statusfolder)
   localfolder.syncmessagesto(statusfolder)
   statusfolder.save()
   localrepos.restore_atime()
   
        

call forgetfolders on local and remote
-}

module Data.Syncable where
import qualified Data.Map as Map

type SyncCollection k v = Map.Map k v

data (Eq k, Ord k, Show k, Show v) => 
    SyncCommand k v = 
           DeleteItem k
         | CopyItem k v
         | ModifyContent k v
    deriving (Eq, Ord, Show)

pairToFunc :: (a -> b -> c) -> (a, b) -> c
pairToFunc func (a, b) = func a b

{- | Perform a bi-directional sync.  Compared to the last known state of
the child, evaluate the new states of the master and child.  Return a list of
changes to make to the master and list of changes to make to the child to
bring them into proper sync.

In the event that both master and child previously had an item, and the payload
of the item has changed on both ends, the payload as given in the child
will take precedence.  If both previously had an item, and it changed on only
one end, the new value "wins".

This relationship should hold:

>let (masterCmds, childCmds) = syncBiDir masterState childState lastChildState
>unaryApplyChanges masterState masterCmds == 
> unaryApplyChanges childState childCmds

This relationship is validated in the test suite that accompanies this
software.

-}
syncBiDir :: (Ord k, Show k, Show v, Eq v) =>
            SyncCollection k v  -- ^ Present state of master
         -> SyncCollection k v  -- ^ Present state of child
         -> SyncCollection k v  -- ^ Last state of child
         -> ([SyncCommand k v], [SyncCommand k v]) -- ^ Changes to make to (master, child)
syncBiDir masterstate childstate lastchildstate =
    (masterchanges, childchanges)
    where masterchanges = (map DeleteItem .
                          findDeleted childstate masterstate $ lastchildstate)
                          ++ 
                          (map (pairToFunc CopyItem) .
                           findAdded childstate masterstate $ lastchildstate)
                          ++ (map (pairToFunc ModifyContent) . Map.toList $ masterPayloadChanges)
          childchanges = (map DeleteItem . 
                          findDeleted masterstate childstate $ lastchildstate)
                         ++
                         (map (pairToFunc CopyItem) .
                          findAdded masterstate childstate $ lastchildstate)
                         ++ (map (pairToFunc ModifyContent) . Map.toList $ childPayloadChanges)
          masterPayloadChanges = 
              Map.union 
                 (findModified masterstate childstate childstate lastchildstate)
                 (findModified masterstate childstate reducedChildState masterstate)
              where reducedChildState = 
                        Map.difference childstate lastchildstate
                 
          -- The child's payload takes precedence, so we are going to
          -- calculate the changes made on the master to apply to the client,
          -- then subtract out any items in the master changes that have the
          -- same key.
          childPayloadChanges = 
              Map.difference (findModified childstate masterstate masterstate lastchildstate)
                 (findModified masterstate childstate childstate lastchildstate)

{- | Compares two SyncCollections, and returns the commands that, when
applied to the first collection, would yield the second. -}
diffCollection :: (Ord k, Show k, Eq v, Show v) => 
                  SyncCollection k v
               -> SyncCollection k v
               -> [SyncCommand k v]
diffCollection coll1 coll2 = 
    (map DeleteItem . findDeleted coll2 coll1 $ coll1) ++
    (map (pairToFunc CopyItem) . findAdded coll2 coll1 $ coll1) ++
    modifiedData
    where modifiedData = 
              map (pairToFunc ModifyContent) .
              Map.toList . 
              Map.mapMaybe id .
              Map.intersectionWith compareFunc coll1 $ coll2
          compareFunc v1 v2
              | v1 /= v2 = Just v2
              | otherwise = Nothing
              
                         
    {-
    (map (pairToFunc ModifyContent) . Map.toList .
         findModified coll1 coll2 $ coll1)
     -}

{- | Returns a list of keys that exist in state2 and lastchildstate
but not in state1 -}
findDeleted :: Ord k =>
               SyncCollection k v -> SyncCollection k v -> SyncCollection k v ->
               [k]
findDeleted state1 state2 lastchildstate =
    Map.keys . Map.difference (Map.intersection state2 lastchildstate) $ state1

{- | Returns a list of keys that exist in state1 but in neither 
state2 nor lastchildstate -}
findAdded :: (Ord k, Eq k) =>
               SyncCollection k v -> SyncCollection k v -> SyncCollection k v ->
               [(k, v)]
findAdded state1 state2 lastchildstate =
    Map.toList . Map.difference state1 . Map.union state2 $ lastchildstate


{- Finds all items that exist in both state1 and lastchildstate in which the payload
is different in state1 than it was in lastchildstate.  Returns the key and new
payload for each such item found. -}
findModified :: (Ord k, Eq v) =>
                SyncCollection k v 
             -> SyncCollection k v
             -> SyncCollection k v
             -> SyncCollection k v
             -> SyncCollection k v
findModified basestate authoritativestate comparisonstate laststate =
    Map.mapMaybe id $
       Map.intersectionWithKey compareFunc comparisonstate laststate
    where compareFunc k compv lastv =
              if lastv == compv
                 then Nothing
                 else case (Map.lookup k basestate, Map.lookup k authoritativestate) of
                        (Nothing, _) -> Nothing
                        (Just basev, Nothing) ->
                            if compv /= basev
                               then Just compv
                               else Nothing
                        (Just basev, Just authv) ->
                            if (authv /= lastv) && (authv /= basev)
                               then Just authv
                               else if compv /= basev && (authv /= basev)
                                    then Just compv
                                    else Nothing

{- | Apply the specified changes to the given SyncCollection.  Returns
a new SyncCollection with the changes applied.  If changes are specified
that would apply to UIDs that do not exist in the source list, these changes
are silently ignored. -}
unaryApplyChanges :: (Eq k, Ord k, Show k, Show v) => 
                     SyncCollection k v -> [SyncCommand k v] -> SyncCollection k v
unaryApplyChanges collection commands =
    let makeChange collection (DeleteItem key) =
            Map.delete key collection
        makeChange collection (CopyItem key val) =
            Map.insert key val collection
        makeChange collection (ModifyContent key val) =
            Map.adjust (\_ -> val) key collection
    in foldl makeChange collection commands
