{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE PackageImports, BangPatterns #-}

module Command.Status where

import "mtl" Control.Monad.State.Strict
import qualified Data.Map as M
import Text.JSON
import Data.Tuple

import Common.Annex
import qualified Types.Backend as B
import qualified Types.Remote as R
import qualified Remote
import qualified Command.Unused
import qualified Git
import qualified Annex
import Command
import Utility.DataUnits
import Utility.DiskFree
import Annex.Content
import Types.Key
import Backend
import Logs.UUID
import Logs.Trust
import Remote
import Config
import Utility.Percentage
import Logs.Transfer
import Types.TrustLevel

-- a named computation that produces a statistic
type Stat = StatState (Maybe (String, StatState String))

-- data about a set of keys
data KeyData = KeyData
	{ countKeys :: Integer
	, sizeKeys :: Integer
	, unknownSizeKeys :: Integer
	, backendsKeys :: M.Map String Integer
	}

-- cached info that multiple Stats use
data StatInfo = StatInfo
	{ presentData :: Maybe KeyData
	, referencedData :: Maybe KeyData
	}

-- a state monad for running Stats in
type StatState = StateT StatInfo Annex

def :: [Command]
def = [command "status" paramNothing seek
	"shows status information about the annex"]

seek :: [CommandSeek]
seek = [withNothing start]

{- Order is significant. Less expensive operations, and operations
 - that share data go together.
 -}
fast_stats :: [Stat]
fast_stats = 
	[ supported_backends
	, supported_remote_types
	, repository_mode
	, remote_list Trusted
	, remote_list SemiTrusted
	, remote_list UnTrusted
	, remote_list DeadTrusted
	, transfer_list
	, disk_size
	]
slow_stats :: [Stat]
slow_stats = 
	[ tmp_size
	, bad_data_size
	, local_annex_keys
	, local_annex_size
	, known_annex_keys
	, known_annex_size
	, bloom_info
	, backend_usage
	]

start :: CommandStart
start = do
	fast <- Annex.getState Annex.fast
	let stats = if fast then fast_stats else fast_stats ++ slow_stats
	showCustom "status" $ do
		evalStateT (mapM_ showStat stats) (StatInfo Nothing Nothing)
		return True
	stop

stat :: String -> (String -> StatState String) -> Stat
stat desc a = return $ Just (desc, a desc)

nostat :: Stat
nostat = return Nothing

json :: JSON j => (j -> String) -> StatState j -> String -> StatState String
json serialize a desc = do
	j <- a
	lift $ maybeShowJSON [(desc, j)]
	return $ serialize j

nojson :: StatState String -> String -> StatState String
nojson a _ = a

showStat :: Stat -> StatState ()
showStat s = maybe noop calc =<< s
  where
	calc (desc, a) = do
		(lift . showHeader) desc
		lift . showRaw =<< a

supported_backends :: Stat
supported_backends = stat "supported backends" $ json unwords $
	return $ map B.name Backend.list

supported_remote_types :: Stat
supported_remote_types = stat "supported remote types" $ json unwords $
	return $ map R.typename Remote.remoteTypes

repository_mode :: Stat
repository_mode = stat "repository mode" $ json id $ lift $
	ifM isDirect 
		( return "direct", return "indirect" )

remote_list :: TrustLevel -> Stat
remote_list level = stat n $ nojson $ lift $ do
	us <- M.keys <$> (M.union <$> uuidMap <*> remoteMap Remote.name)
	rs <- fst <$> trustPartition level us
	s <- prettyPrintUUIDs n rs
	return $ if null s then "0" else show (length rs) ++ "\n" ++ beginning s
  where
	n = showTrustLevel level ++ " repositories"
	
local_annex_size :: Stat
local_annex_size = stat "local annex size" $ json id $
	showSizeKeys <$> cachedPresentData

local_annex_keys :: Stat
local_annex_keys = stat "local annex keys" $ json show $
	countKeys <$> cachedPresentData

known_annex_size :: Stat
known_annex_size = stat "known annex size" $ json id $
	showSizeKeys <$> cachedReferencedData

known_annex_keys :: Stat
known_annex_keys = stat "known annex keys" $ json show $
	countKeys <$> cachedReferencedData

tmp_size :: Stat
tmp_size = staleSize "temporary directory size" gitAnnexTmpDir

bad_data_size :: Stat
bad_data_size = staleSize "bad keys size" gitAnnexBadDir

bloom_info :: Stat
bloom_info = stat "bloom filter size" $ json id $ do
	localkeys <- countKeys <$> cachedPresentData
	capacity <- fromIntegral <$> lift Command.Unused.bloomCapacity
	let note = aside $
		if localkeys >= capacity
		then "appears too small for this repository; adjust annex.bloomcapacity"
		else showPercentage 1 (percentage capacity localkeys) ++ " full"

	-- Two bloom filters are used at the same time, so double the size
	-- of one.
	size <- roughSize memoryUnits False . (* 2) . fromIntegral . fst <$>
		lift Command.Unused.bloomBitsHashes

	return $ size ++ note

transfer_list :: Stat
transfer_list = stat "transfers in progress" $ nojson $ lift $ do
	uuidmap <- Remote.remoteMap id
	ts <- getTransfers
	if null ts
		then return "none"
		else return $ multiLine $
			map (\(t, i) -> line uuidmap t i) $ sort ts
  where
	line uuidmap t i = unwords
		[ showLcDirection (transferDirection t) ++ "ing"
		, fromMaybe (key2file $ transferKey t) (associatedFile i)
		, if transferDirection t == Upload then "to" else "from"
		, maybe (fromUUID $ transferUUID t) Remote.name $
			M.lookup (transferUUID t) uuidmap
		]

disk_size :: Stat
disk_size = stat "available local disk space" $ json id $ lift $
	calcfree
		<$> (annexDiskReserve <$> Annex.getGitConfig)
		<*> inRepo (getDiskFree . gitAnnexDir)
  where
	calcfree reserve (Just have) = unwords
		[ roughSize storageUnits False $ nonneg $ have - reserve
		, "(+" ++ roughSize storageUnits False reserve
		, "reserved)"
		]			
	calcfree _ _ = "unknown"

	nonneg x
		| x >= 0 = x
		| otherwise = 0

backend_usage :: Stat
backend_usage = stat "backend usage" $ nojson $
	calc
		<$> (backendsKeys <$> cachedReferencedData)
		<*> (backendsKeys <$> cachedPresentData)
  where
	calc x y = multiLine $
		map (\(n, b) -> b ++ ": " ++ show n) $
		reverse $ sort $ map swap $ M.toList $
		M.unionWith (+) x y

cachedPresentData :: StatState KeyData
cachedPresentData = do
	s <- get
	case presentData s of
		Just v -> return v
		Nothing -> do
			v <- foldKeys <$> lift getKeysPresent
			put s { presentData = Just v }
			return v

cachedReferencedData :: StatState KeyData
cachedReferencedData = do
	s <- get
	case referencedData s of
		Just v -> return v
		Nothing -> do
			!v <- lift $ Command.Unused.withKeysReferenced
				emptyKeyData addKey
			put s { referencedData = Just v }
			return v

emptyKeyData :: KeyData
emptyKeyData = KeyData 0 0 0 M.empty

foldKeys :: [Key] -> KeyData
foldKeys = foldl' (flip addKey) emptyKeyData

addKey :: Key -> KeyData -> KeyData
addKey key (KeyData count size unknownsize backends) =
	KeyData count' size' unknownsize' backends'
  where
	{- All calculations strict to avoid thunks when repeatedly
	 - applied to many keys. -}
	!count' = count + 1
	!backends' = M.insertWith' (+) (keyBackendName key) 1 backends
	!size' = maybe size (+ size) ks
	!unknownsize' = maybe (unknownsize + 1) (const unknownsize) ks
	ks = keySize key

showSizeKeys :: KeyData -> String
showSizeKeys d = total ++ missingnote
  where
	total = roughSize storageUnits False $ sizeKeys d
	missingnote
		| unknownSizeKeys d == 0 = ""
		| otherwise = aside $
			"+ " ++ show (unknownSizeKeys d) ++
			" keys of unknown size"

staleSize :: String -> (Git.Repo -> FilePath) -> Stat
staleSize label dirspec = go =<< lift (Command.Unused.staleKeys dirspec)
  where
	go [] = nostat
	go keys = onsize =<< sum <$> keysizes keys
	onsize 0 = nostat
	onsize size = stat label $
		json (++ aside "clean up with git-annex unused") $
			return $ roughSize storageUnits False size
	keysizes keys = map (fromIntegral . fileSize) <$> stats keys
	stats keys = do
		dir <- lift $ fromRepo dirspec
		liftIO $ forM keys $ \k -> getFileStatus (dir </> keyFile k)

aside :: String -> String
aside s = " (" ++ s ++ ")"

multiLine :: [String] -> String
multiLine = concatMap (\l -> "\n\t" ++ l)

