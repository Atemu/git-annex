{- Helper to make remotes support export and import (or not).
 -
 - Copyright 2017-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Remote.Helper.ExportImport where

import Annex.Common
import Types.Remote
import Types.Backend
import Types.Key
import Backend
import Remote.Helper.Encryptable (isEncrypted)
import qualified Database.Export as Export
import qualified Database.ContentIdentifier as ContentIdentifier
import Annex.Export
import Annex.LockFile
import Config
import Git.Types (fromRef)
import Logs.Export
import Logs.ContentIdentifier (recordContentIdentifier)

import qualified Data.Map as M
import Control.Concurrent.STM

-- | Use for remotes that do not support exports.
class HasExportUnsupported a where
	exportUnsupported :: a

instance HasExportUnsupported (RemoteConfig -> RemoteGitConfig -> Annex Bool) where
	exportUnsupported = \_ _ -> return False

instance HasExportUnsupported (ExportActions Annex) where
	exportUnsupported = ExportActions
		{ storeExport = \_ _ _ _ -> do
			warning "store export is unsupported"
			return False
		, retrieveExport = \_ _ _ _ -> return False
		, checkPresentExport = \_ _ -> return False
		, removeExport = \_ _ -> return False
		, removeExportDirectory = Just $ \_ -> return False
		, renameExport = \_ _ _ -> return Nothing
		}

-- | Use for remotes that do not support imports.
class HasImportUnsupported a where
	importUnsupported :: a

instance HasImportUnsupported (RemoteConfig -> RemoteGitConfig -> Annex Bool) where
	importUnsupported = \_ _ -> return False

instance HasImportUnsupported (ImportActions Annex) where
	importUnsupported = ImportActions
		{ listImportableContents = return Nothing
		, retrieveExportWithContentIdentifier = \_ _ _ _ _ -> return Nothing
		, storeExportWithContentIdentifier = \_ _ _ _ _ -> return Nothing
		, removeExportWithContentIdentifier = \_ _ _ -> return False
		, removeExportDirectoryWhenEmpty = Just $ \_ -> return False
		, checkPresentExportWithContentIdentifier = \_ _ _ -> return False
		}

exportIsSupported :: RemoteConfig -> RemoteGitConfig -> Annex Bool
exportIsSupported = \_ _ -> return True

importIsSupported :: RemoteConfig -> RemoteGitConfig -> Annex Bool
importIsSupported = \_ _ -> return True

-- | Prevent or allow exporttree=yes and importtree=yes when
-- setting up a new remote, depending on exportSupported and importSupported.
adjustExportImportRemoteType :: RemoteType -> RemoteType
adjustExportImportRemoteType rt = rt { setup = setup' }
  where
	setup' st mu cp c gc =
		let checkconfig supported configured setting cont =
			ifM (supported rt c gc)
				( case st of
					Init
						| configured c && isEncrypted c ->
							giveup $ "cannot enable both encryption and " ++ setting
						| otherwise -> cont
					Enable oldc
						| configured c /= configured oldc ->
							giveup $ "cannot change " ++ setting ++ " of existing special remote"
						| otherwise -> cont
				, if configured c
					then giveup $ setting ++ " is not supported by this special remote"
					else cont
				)
		in checkconfig exportSupported exportTree "exporttree" $
			checkconfig importSupported importTree "importtree" $
				if importTree c && not (exportTree c)
					then giveup "cannot enable importtree=yes without also enabling exporttree=yes"
					else setup rt st mu cp c gc

-- | Adjust a remote to support exporttree=yes and importree=yes.
--
-- Note that all remotes with importree=yes also have exporttree=yes.
adjustExportImport :: Remote -> Annex Remote
adjustExportImport r = case M.lookup "exporttree" (config r) of
	Nothing -> return $ notexport r
	Just c -> case yesNo c of
		Just True -> ifM (isExportSupported r)
			( do
				exportdbv <- prepexportdb
				r' <- isexport exportdbv
				if importTree (config r)
					then isimport r' exportdbv
					else return r'
			, return $ notexport r
			)
		Just False -> return $ notexport r
		Nothing -> do
			warning $ "bad exporttree value for " ++ name r ++ ", assuming not an export"
			return $ notexport r
  where
	notexport r' = notimport r'
		{ exportActions = exportUnsupported
		, remotetype = (remotetype r')
			{ exportSupported = exportUnsupported
			}
		}
	
	notimport r' = r'
		{ importActions = importUnsupported
		, remotetype = (remotetype r')
			{ importSupported = importUnsupported
			}
		}
	
	isimport r' exportdbv = do
		ciddbv <- prepciddb

		let keycids k = do
			db <- getciddb ciddbv
			liftIO $ ContentIdentifier.getContentIdentifiers db (uuid r') k

		let checkpresent k loc = 
			checkPresentExportWithContentIdentifier
				(importActions r')
				k loc 
				=<< keycids k

		return $ r'
			{ exportActions = (exportActions r')
				{ storeExport = \f k loc p -> do
					db <- getciddb ciddbv
					exportdb <- getexportdb exportdbv
					updateexportdb exportdb exportdbv
					oldks <- liftIO $ Export.getExportTreeKey exportdb loc
					oldcids <- liftIO $ concat
						<$> mapM (ContentIdentifier.getContentIdentifiers db (uuid r')) oldks
					storeExportWithContentIdentifier (importActions r') f k loc oldcids p >>= \case
						Nothing -> return False
						Just newcid -> do
							withExclusiveLock gitAnnexContentIdentifierLock $ do
								liftIO $ ContentIdentifier.recordContentIdentifier db (uuid r') newcid k
								liftIO $ ContentIdentifier.flushDbQueue db
							recordContentIdentifier (uuid r') newcid k
							return True
				, removeExport = \k loc ->
					removeExportWithContentIdentifier (importActions r') k loc
						=<< keycids k
				, removeExportDirectory = removeExportDirectoryWhenEmpty (importActions r')
				-- renameExport is optional, and the
				-- remote's implementation may
				-- lose modifications to the file
				-- (by eg copying and then deleting)
				-- so don't use it
				, renameExport = \_ _ _ -> return Nothing
				, checkPresentExport = checkpresent
				}
			, checkPresent = if appendonly r'
				then checkPresent r'
				else \k -> anyM (checkpresent k)
					=<< getexportlocs exportdbv k
			}

	isexport dbv = return $ r
		-- Storing a key on an export could be implemented,
		-- but it would perform unncessary work
		-- when another repository has already stored the
		-- key, and the local repository does not know
		-- about it. To avoid unnecessary costs, don't do it.
		{ storeKey = \_ _ _ -> do
			warning "remote is configured with exporttree=yes; use `git-annex export` to store content on it"
			return False
		-- Keys can be retrieved using retrieveExport, 
		-- but since that retrieves from a path in the
		-- remote that another writer could have replaced
		-- with content not of the requested key,
		-- the content has to be strongly verified.
		--
		-- appendonly remotes have a key/value store,
		-- so don't need to use retrieveExport. However,
		-- fall back to it if retrieveKeyFile fails.
		, retrieveKeyFile = \k af dest p ->
			let retrieveexport = retrieveKeyFileFromExport dbv k af dest p
			in if appendonly r
				then do
					ret@(ok, _v) <- retrieveKeyFile r k af dest p
					if ok
						then return ret
						else retrieveexport
				else retrieveexport
		, retrieveKeyFileCheap = if appendonly r
			then retrieveKeyFileCheap r
			else \_ _ _ -> return False
		-- Removing a key from an export would need to
		-- change the tree in the export log to not include
		-- the file. Otherwise, conflicts when removing
		-- files would not be dealt with correctly.
		-- There does not seem to be a good use case for
		-- removing a key from an export in any case.
		, removeKey = \_k -> do
			warning "dropping content from an export is not supported; use `git annex export` to export a tree that lacks the files you want to remove"
			return False
		-- Can't lock content on exports, since they're
		-- not key/value stores, and someone else could
		-- change what's exported to a file at any time.
		--
		-- (except for appendonly remotes)
		, lockContent = if appendonly r
			then lockContent r
			else Nothing
		-- Check if any of the files a key was exported to
		-- are present. This doesn't guarantee the export
		-- contains the right content, which is why export
		-- remotes are untrusted.
		--
		-- (but appendonly remotes work the same as any
		-- non-export remote)
		, checkPresent = if appendonly r
			then checkPresent r
			else \k -> anyM (checkPresentExport (exportActions r) k)
				=<< getexportlocs dbv k
		-- checkPresent from an export is more expensive
		-- than otherwise, so not cheap. Also, this
		-- avoids things that look at checkPresentCheap and
		-- silently skip non-present files from behaving
		-- in confusing ways when there's an export
		-- conflict.
		, checkPresentCheap = False
		, mkUnavailable = return Nothing
		, getInfo = do
			ts <- map fromRef . exportedTreeishes
				<$> getExport (uuid r)
			is <- getInfo r
			return (is++[("export", "yes"), ("exportedtree", unwords ts)])
		}

	prepciddb = do
		lcklckv <- liftIO newEmptyTMVarIO
		dbtv <- liftIO newEmptyTMVarIO
		return (dbtv, lcklckv)
	
	prepexportdb = do
		lcklckv <- liftIO newEmptyTMVarIO
		dbv <- liftIO newEmptyTMVarIO
		exportinconflict <- liftIO $ newTVarIO False
		exportupdated <- liftIO $ newTMVarIO ()
		return (dbv, lcklckv, exportinconflict, exportupdated)

	-- Only open the database once it's needed.
	getciddb (dbtv, lcklckv) =
		liftIO (atomically (tryReadTMVar dbtv)) >>= \case
			Just db -> return db
			-- let only one thread take the lock
			Nothing -> ifM (liftIO $ atomically $ tryPutTMVar lcklckv ())
				( do
					db <- ContentIdentifier.openDb
					ContentIdentifier.needsUpdateFromLog db >>= \case
						Just v -> withExclusiveLock gitAnnexContentIdentifierLock $
							ContentIdentifier.updateFromLog db v
						Nothing -> noop
					liftIO $ atomically $ putTMVar dbtv db
					return db
				-- loser waits for winner to open the db and
				-- can then also use its handle
				, liftIO $ atomically (readTMVar dbtv)
				)
	
	-- Only open the database once it's needed.
	getexportdb (dbv, lcklckv, _, _) = 
		liftIO (atomically (tryReadTMVar dbv)) >>= \case
			Just db -> return db
			-- let only one thread take the lock
			Nothing -> ifM (liftIO $ atomically $ tryPutTMVar lcklckv ())
				( do
					db <- Export.openDb (uuid r)
					liftIO $ atomically $ putTMVar dbv db
					return db
				-- loser waits for winner to open the db and
				-- can then also use its handle
				, liftIO $ atomically (readTMVar dbv)
				)

	getexportinconflict (_, _, v, _) = v

	-- Check once if the export log is different than the database and
	-- updates the database, to notice when an export has been
	-- updated from another repository.				
	updateexportdb db (_, _, exportinconflict, exportupdated) =
		liftIO (atomically (tryTakeTMVar exportupdated)) >>= \case
			Just () -> Export.updateExportTreeFromLog db >>= \case
				Export.ExportUpdateSuccess -> return ()
				Export.ExportUpdateConflict -> do
					warnExportConflict r
					liftIO $ atomically $
						writeTVar exportinconflict True
			Nothing -> return ()
		
	getexportlocs dbv k = do
		db <- getexportdb dbv
		updateexportdb db dbv
		liftIO $ Export.getExportTree db k

	retrieveKeyFileFromExport dbv k _af dest p = unVerified $
		if maybe False (isJust . verifyKeyContent) (maybeLookupBackendVariety (keyVariety k))
			then do
				locs <- getexportlocs dbv k
				case locs of
					[] -> do
						ifM (liftIO $ atomically $ readTVar $ getexportinconflict dbv)
							( warning "unknown export location, likely due to the export conflict"
							, warning "unknown export location"
							)
						return False
					(l:_) -> retrieveExport (exportActions r) k l dest p
			else do
				warning $ "exported content cannot be verified due to using the " ++ decodeBS (formatKeyVariety (keyVariety k)) ++ " backend"
				return False
