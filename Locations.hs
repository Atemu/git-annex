{- git-annex file locations
 -
 - Copyright 2010-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Locations (
	keyFile,
	fileKey,
	keyPaths,
	keyPath,
	annexDir,
	objectDir,
	gitAnnexLocation,
	gitAnnexLink,
	gitAnnexContentLock,
	gitAnnexMapping,
	gitAnnexInodeCache,
	gitAnnexInodeSentinal,
	gitAnnexInodeSentinalCache,
	annexLocations,
	annexLocation,
	gitAnnexDir,
	gitAnnexObjectDir,
	gitAnnexTmpMiscDir,
	gitAnnexTmpObjectDir,
	gitAnnexTmpObjectLocation,
	gitAnnexBadDir,
	gitAnnexBadLocation,
	gitAnnexUnusedLog,
	gitAnnexFsckState,
	gitAnnexFsckResultsLog,
	gitAnnexScheduleState,
	gitAnnexTransferDir,
	gitAnnexCredsDir,
	gitAnnexWebCertificate,
	gitAnnexWebPrivKey,
	gitAnnexFeedStateDir,
	gitAnnexFeedState,
	gitAnnexMergeDir,
	gitAnnexJournalDir,
	gitAnnexJournalLock,
	gitAnnexPreCommitLock,
	gitAnnexMergeLock,
	gitAnnexIndex,
	gitAnnexIndexStatus,
	gitAnnexViewIndex,
	gitAnnexViewLog,
	gitAnnexIgnoredRefs,
	gitAnnexPidFile,
	gitAnnexDaemonStatusFile,
	gitAnnexLogFile,
	gitAnnexFuzzTestLogFile,
	gitAnnexHtmlShim,
	gitAnnexUrlFile,
	gitAnnexTmpCfgFile,
	gitAnnexSshDir,
	gitAnnexRemotesDir,
	gitAnnexAssistantDefaultDir,
	isLinkToAnnex,
	annexHashes,
	hashDirMixed,
	hashDirLower,
	preSanitizeKeyName,

	prop_idempotent_fileKey
) where

import Data.Bits
import Data.Word
import Data.Hash.MD5
import Data.Char

import Common
import Types
import Types.Key
import Types.UUID
import qualified Git

{- Conventions:
 -
 - Functions ending in "Dir" should always return values ending with a
 - trailing path separator. Most code does not rely on that, but a few
 - things do. 
 -
 - Everything else should not end in a trailing path sepatator. 
 -
 - Only functions (with names starting with "git") that build a path
 - based on a git repository should return an absolute path.
 - Everything else should use relative paths.
 -}

{- The directory git annex uses for local state, relative to the .git
 - directory -}
annexDir :: FilePath
annexDir = addTrailingPathSeparator "annex"

{- The directory git annex uses for locally available object content,
 - relative to the .git directory -}
objectDir :: FilePath
objectDir = addTrailingPathSeparator $ annexDir </> "objects"

{- Annexed file's possible locations relative to the .git directory.
 - There are two different possibilities, using different hashes. -}
annexLocations :: Key -> [FilePath]
annexLocations key = map (annexLocation key) annexHashes
annexLocation :: Key -> Hasher -> FilePath
annexLocation key hasher = objectDir </> keyPath key hasher

{- Annexed object's absolute location in a repository.
 -
 - When there are multiple possible locations, returns the one where the
 - file is actually present.
 -
 - When the file is not present, returns the location where the file should
 - be stored.
 -
 - This does not take direct mode into account, so in direct mode it is not
 - the actual location of the file's content.
 -}
gitAnnexLocation :: Key -> Git.Repo -> GitConfig -> IO FilePath
gitAnnexLocation key r config = gitAnnexLocation' key r (annexCrippledFileSystem config)
gitAnnexLocation' :: Key -> Git.Repo -> Bool -> IO FilePath
gitAnnexLocation' key r crippled
	{- Bare repositories default to hashDirLower for new
	 - content, as it's more portable.
	 -
	 - Repositories on filesystems that are crippled also use
	 - hashDirLower, since they do not use symlinks and it's
	 - more portable. -}
	| Git.repoIsLocalBare r || crippled =
		check $ map inrepo $ annexLocations key
	{- Non-bare repositories only use hashDirMixed, so
	 - don't need to do any work to check if the file is
	 - present. -}
	| otherwise = return $ inrepo $ annexLocation key hashDirMixed
  where
	inrepo d = Git.localGitDir r </> d
	check locs@(l:_) = fromMaybe l <$> firstM doesFileExist locs
	check [] = error "internal"

{- Calculates a symlink to link a file to an annexed object. -}
gitAnnexLink :: FilePath -> Key -> Git.Repo -> IO FilePath
gitAnnexLink file key r = do
	currdir <- getCurrentDirectory
	let absfile = fromMaybe whoops $ absNormPathUnix currdir file
	loc <- gitAnnexLocation' key r False
	return $ relPathDirToFile (parentDir absfile) loc
  where
  	whoops = error $ "unable to normalize " ++ file

{- File used to lock a key's content. -}
gitAnnexContentLock :: Key -> Git.Repo -> GitConfig -> IO FilePath
gitAnnexContentLock key r config = do
	loc <- gitAnnexLocation key r config
	return $ loc ++ ".lck"

{- File that maps from a key to the file(s) in the git repository.
 - Used in direct mode. -}
gitAnnexMapping :: Key -> Git.Repo -> GitConfig -> IO FilePath
gitAnnexMapping key r config = do
	loc <- gitAnnexLocation key r config
	return $ loc ++ ".map"

{- File that caches information about a key's content, used to determine
 - if a file has changed.
 - Used in direct mode. -}
gitAnnexInodeCache :: Key -> Git.Repo -> GitConfig -> IO FilePath
gitAnnexInodeCache key r config  = do
	loc <- gitAnnexLocation key r config
	return $ loc ++ ".cache"

gitAnnexInodeSentinal :: Git.Repo -> FilePath
gitAnnexInodeSentinal r = gitAnnexDir r </> "sentinal"

gitAnnexInodeSentinalCache :: Git.Repo -> FilePath
gitAnnexInodeSentinalCache r = gitAnnexInodeSentinal r ++ ".cache"

{- The annex directory of a repository. -}
gitAnnexDir :: Git.Repo -> FilePath
gitAnnexDir r = addTrailingPathSeparator $ Git.localGitDir r </> annexDir

{- The part of the annex directory where file contents are stored. -}
gitAnnexObjectDir :: Git.Repo -> FilePath
gitAnnexObjectDir r = addTrailingPathSeparator $ Git.localGitDir r </> objectDir

{- .git/annex/misctmp/ is used for random temp files -}
gitAnnexTmpMiscDir :: Git.Repo -> FilePath
gitAnnexTmpMiscDir r = addTrailingPathSeparator $ gitAnnexDir r </> "misctmp"

{- .git/annex/tmp/ is used for temp files for key's contents -}
gitAnnexTmpObjectDir :: Git.Repo -> FilePath
gitAnnexTmpObjectDir r = addTrailingPathSeparator $ gitAnnexDir r </> "tmp"

{- The temp file to use for a given key's content. -}
gitAnnexTmpObjectLocation :: Key -> Git.Repo -> FilePath
gitAnnexTmpObjectLocation key r = gitAnnexTmpObjectDir r </> keyFile key

{- .git/annex/bad/ is used for bad files found during fsck -}
gitAnnexBadDir :: Git.Repo -> FilePath
gitAnnexBadDir r = addTrailingPathSeparator $ gitAnnexDir r </> "bad"

{- The bad file to use for a given key. -}
gitAnnexBadLocation :: Key -> Git.Repo -> FilePath
gitAnnexBadLocation key r = gitAnnexBadDir r </> keyFile key

{- .git/annex/foounused is used to number possibly unused keys -}
gitAnnexUnusedLog :: FilePath -> Git.Repo -> FilePath
gitAnnexUnusedLog prefix r = gitAnnexDir r </> (prefix ++ "unused")

{- .git/annex/fsckstate is used to store information about incremental fscks. -}
gitAnnexFsckState :: Git.Repo -> FilePath
gitAnnexFsckState r = gitAnnexDir r </> "fsckstate"

{- .git/annex/fsckresults/uuid is used to store results of git fscks -}
gitAnnexFsckResultsLog :: UUID -> Git.Repo -> FilePath
gitAnnexFsckResultsLog u r = gitAnnexDir r </> "fsckresults" </> fromUUID u

{- .git/annex/schedulestate is used to store information about when
 - scheduled jobs were last run. -}
gitAnnexScheduleState :: Git.Repo -> FilePath
gitAnnexScheduleState r = gitAnnexDir r </> "schedulestate"

{- .git/annex/creds/ is used to store credentials to access some special
 - remotes. -}
gitAnnexCredsDir :: Git.Repo -> FilePath
gitAnnexCredsDir r = addTrailingPathSeparator $ gitAnnexDir r </> "creds"

{- .git/annex/certificate.pem and .git/annex/key.pem are used by the webapp
 - when HTTPS is enabled -}
gitAnnexWebCertificate :: Git.Repo -> FilePath
gitAnnexWebCertificate r = gitAnnexDir r </> "certificate.pem"
gitAnnexWebPrivKey :: Git.Repo -> FilePath
gitAnnexWebPrivKey r = gitAnnexDir r </> "privkey.pem"

{- .git/annex/feeds/ is used to record per-key (url) state by importfeeds -}
gitAnnexFeedStateDir :: Git.Repo -> FilePath
gitAnnexFeedStateDir r = addTrailingPathSeparator $ gitAnnexDir r </> "feedstate"

gitAnnexFeedState :: Key -> Git.Repo -> FilePath
gitAnnexFeedState k r = gitAnnexFeedStateDir r </> keyFile k

{- .git/annex/merge/ is used for direct mode merges. -}
gitAnnexMergeDir :: Git.Repo -> FilePath
gitAnnexMergeDir r = addTrailingPathSeparator $ gitAnnexDir r </> "merge"

{- .git/annex/transfer/ is used to record keys currently
 - being transferred, and other transfer bookkeeping info. -}
gitAnnexTransferDir :: Git.Repo -> FilePath
gitAnnexTransferDir r = addTrailingPathSeparator $ gitAnnexDir r </> "transfer"

{- .git/annex/journal/ is used to journal changes made to the git-annex
 - branch -}
gitAnnexJournalDir :: Git.Repo -> FilePath
gitAnnexJournalDir r = addTrailingPathSeparator $ gitAnnexDir r </> "journal"

{- Lock file for the journal. -}
gitAnnexJournalLock :: Git.Repo -> FilePath
gitAnnexJournalLock r = gitAnnexDir r </> "journal.lck"

{- Lock file for the pre-commit hook. -}
gitAnnexPreCommitLock :: Git.Repo -> FilePath
gitAnnexPreCommitLock r = gitAnnexDir r </> "precommit.lck"

{- Lock file for direct mode merge. -}
gitAnnexMergeLock :: Git.Repo -> FilePath
gitAnnexMergeLock r = gitAnnexDir r </> "merge.lck"

{- .git/annex/index is used to stage changes to the git-annex branch -}
gitAnnexIndex :: Git.Repo -> FilePath
gitAnnexIndex r = gitAnnexDir r </> "index"

{- Holds the ref of the git-annex branch that the index was last updated to.
 -
 - The .lck in the name is a historical accident; this is not used as a
 - lock. -}
gitAnnexIndexStatus :: Git.Repo -> FilePath
gitAnnexIndexStatus r = gitAnnexDir r </> "index.lck"

{- The index file used to generate a filtered branch view._-}
gitAnnexViewIndex :: Git.Repo -> FilePath
gitAnnexViewIndex r = gitAnnexDir r </> "viewindex"

{- File containing a log of recently accessed views. -}
gitAnnexViewLog :: Git.Repo -> FilePath
gitAnnexViewLog r = gitAnnexDir r </> "viewlog"

{- List of refs that should not be merged into the git-annex branch. -}
gitAnnexIgnoredRefs :: Git.Repo -> FilePath
gitAnnexIgnoredRefs r = gitAnnexDir r </> "ignoredrefs"

{- Pid file for daemon mode. -}
gitAnnexPidFile :: Git.Repo -> FilePath
gitAnnexPidFile r = gitAnnexDir r </> "daemon.pid"

{- Status file for daemon mode. -}
gitAnnexDaemonStatusFile :: Git.Repo -> FilePath
gitAnnexDaemonStatusFile r = gitAnnexDir r </> "daemon.status"

{- Log file for daemon mode. -}
gitAnnexLogFile :: Git.Repo -> FilePath
gitAnnexLogFile r = gitAnnexDir r </> "daemon.log"

{- Log file for fuzz test. -}
gitAnnexFuzzTestLogFile :: Git.Repo -> FilePath
gitAnnexFuzzTestLogFile r = gitAnnexDir r </> "fuzztest.log"

{- Html shim file used to launch the webapp. -}
gitAnnexHtmlShim :: Git.Repo -> FilePath
gitAnnexHtmlShim r = gitAnnexDir r </> "webapp.html"

{- File containing the url to the webapp. -}
gitAnnexUrlFile :: Git.Repo -> FilePath
gitAnnexUrlFile r = gitAnnexDir r </> "url"

{- Temporary file used to edit configuriation from the git-annex branch. -}
gitAnnexTmpCfgFile :: Git.Repo -> FilePath
gitAnnexTmpCfgFile r = gitAnnexDir r </> "config.tmp"

{- .git/annex/ssh/ is used for ssh connection caching -}
gitAnnexSshDir :: Git.Repo -> FilePath
gitAnnexSshDir r = addTrailingPathSeparator $ gitAnnexDir r </> "ssh"

{- .git/annex/remotes/ is used for remote-specific state. -}
gitAnnexRemotesDir :: Git.Repo -> FilePath
gitAnnexRemotesDir r = addTrailingPathSeparator $ gitAnnexDir r </> "remotes"

{- This is the base directory name used by the assistant when making
 - repositories, by default. -}
gitAnnexAssistantDefaultDir :: FilePath
gitAnnexAssistantDefaultDir = "annex"

{- Checks a symlink target to see if it appears to point to annexed content.
 -
 - We only look at paths inside the .git directory, and not at the .git
 - directory itself, because GIT_DIR may cause a directory name other
 - than .git to be used.
 -}
isLinkToAnnex :: FilePath -> Bool
isLinkToAnnex s = (pathSeparator:objectDir) `isInfixOf` s

{- Sanitizes a String that will be used as part of a Key's keyName,
 - dealing with characters that cause problems on substandard filesystems.
 -
 - This is used when a new Key is initially being generated, eg by getKey.
 - Unlike keyFile and fileKey, it does not need to be a reversable
 - escaping. Also, it's ok to change this to add more problimatic
 - characters later. Unlike changing keyFile, which could result in the
 - filenames used for existing keys changing and contents getting lost.
 -
 - It is, however, important that the input and output of this function
 - have a 1:1 mapping, to avoid two different inputs from mapping to the
 - same key.
 -}
preSanitizeKeyName :: String -> String
preSanitizeKeyName = concatMap escape
  where
  	escape c
		| isAsciiUpper c || isAsciiLower c || isDigit c = [c]
		| c `elem` ".-_ " = [c] -- common, assumed safe
		| c `elem` "/%:" = [c] -- handled by keyFile
		-- , is safe and uncommon, so will be used to escape
		-- other characters. By itself, it is escaped to 
		-- doubled form.
		| c == ',' = ",,"
		| otherwise = ',' : show (ord c)

{- Converts a key into a filename fragment without any directory.
 -
 - Escape "/" in the key name, to keep a flat tree of files and avoid
 - issues with keys containing "/../" or ending with "/" etc. 
 -
 - "/" is escaped to "%" because it's short and rarely used, and resembles
 -     a slash
 - "%" is escaped to "&s", and "&" to "&a"; this ensures that the mapping
 -     is one to one.
 - ":" is escaped to "&c", because it seemed like a good idea at the time.
 -
 - Changing what this function escapes and how is not a good idea, as it
 - can cause existing objects to get lost.
 -}
keyFile :: Key -> FilePath
keyFile key = replace "/" "%" $ replace ":" "&c" $
	replace "%" "&s" $ replace "&" "&a"  $ key2file key

{- Reverses keyFile, converting a filename fragment (ie, the basename of
 - the symlink target) into a key. -}
fileKey :: FilePath -> Maybe Key
fileKey file = file2key $
	replace "&a" "&" $ replace "&s" "%" $
		replace "&c" ":" $ replace "%" "/" file

{- for quickcheck -}
prop_idempotent_fileKey :: String -> Bool
prop_idempotent_fileKey s
	| null s = True -- it's not legal for a key to have no keyName
	| otherwise= Just k == fileKey (keyFile k)
  where
	k = stubKey { keyName = s, keyBackendName = "test" }

{- A location to store a key on the filesystem. A directory hash is used,
 - to protect against filesystems that dislike having many items in a
 - single directory.
 -
 - The file is put in a directory with the same name, this allows
 - write-protecting the directory to avoid accidental deletion of the file.
 -}
keyPath :: Key -> Hasher -> FilePath
keyPath key hasher = hasher key </> f </> f
  where
	f = keyFile key

{- All possibile locations to store a key using different directory hashes. -}
keyPaths :: Key -> [FilePath]
keyPaths key = map (keyPath key) annexHashes

{- Two different directory hashes may be used. The mixed case hash
 - came first, and is fine, except for the problem of case-strict
 - filesystems such as Linux VFAT (mounted with shortname=mixed),
 - which do not allow using a directory "XX" when "xx" already exists.
 - To support that, most repositories use the lower case hash for new data. -}
type Hasher = Key -> FilePath
annexHashes :: [Hasher]
annexHashes = [hashDirLower, hashDirMixed]

hashDirMixed :: Hasher
hashDirMixed k = addTrailingPathSeparator $ take 2 dir </> drop 2 dir
  where
	dir = take 4 $ display_32bits_as_dir =<< [a,b,c,d]
	ABCD (a,b,c,d) = md5 $ md5FilePath $ key2file k

hashDirLower :: Hasher
hashDirLower k = addTrailingPathSeparator $ take 3 dir </> drop 3 dir
  where
	dir = take 6 $ md5s $ md5FilePath $ key2file k

{- modified version of display_32bits_as_hex from Data.Hash.MD5
 -   Copyright (C) 2001 Ian Lynagh 
 -   License: Either BSD or GPL
 -}
display_32bits_as_dir :: Word32 -> String
display_32bits_as_dir w = trim $ swap_pairs cs
  where 
	-- Need 32 characters to use. To avoid inaverdently making
	-- a real word, use letters that appear less frequently.
	chars = ['0'..'9'] ++ "zqjxkmvwgpfZQJXKMVWGPF"
	cs = map (\x -> getc $ (shiftR w (6*x)) .&. 31) [0..7]
	getc n = chars !! fromIntegral n
	swap_pairs (x1:x2:xs) = x2:x1:swap_pairs xs
	swap_pairs _ = []
	-- Last 2 will always be 00, so omit.
	trim = take 6
