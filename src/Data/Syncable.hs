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

type SyncCollection k = Map.Map k ()

data (Eq k, Ord k, Show k) => 
    SyncCommand k = 
           DeleteItem k
         | CopyItem k
    deriving (Eq, Ord, Show)

syncThem :: (Ord k, Show k) =>
            SyncCollection k  -- ^ Present state of master
         -> SyncCollection k  -- ^ Present state of child
         -> SyncCollection k  -- ^ Last state of child
         -> ([SyncCommand k], [SyncCommand k], [SyncCommand k]) -- ^ Changes to make to (master, child, child state repo)
syncThem masterstate childstate lastchildstate =
    (masterchanges, childchanges, statuschanges)
    where masterchanges = [] 
          childchanges = map DeleteItem masterToChildDeletes
          statuschanges = []

{-
        # Delete local copies of remote messages.  This way,
        # if a message's flag is modified locally but it has been
        # deleted remotely, we'll delete it locally.  Otherwise, we
        # try to modify a deleted message's flags!  This step
        # need only be taken if a statusfolder is present; otherwise,
        # there is no action taken *to* the remote repository.

        FIXME: validate logic in situation of new folder here -}

          masterToChildDeletes = syncToDelete masterstate childstate

{- | Returns a list of keys that exist in masterstate but not in childstate -}
syncToDelete :: (Ord k) => 
                SyncCollection k -> SyncCollection k -> [k]
syncToDelete masterstate childstate = 
    concatMap keyfunc (Map.keys masterstate)
    where keyfunc k = 
              case Map.lookup k childstate of
                Nothing -> [k]
                Just _ -> []

{- | Returns a list of keys that exist in the passed state -}
filterKeys :: (Ord k) => 
              SyncCollection k -> [k] -> [k]
filterKeys state keylist =
    concatMap keyfunc keylist
    where keyfunc k =
              case Map.lookup k state of
                Nothing -> []
                Just _ -> [k]
