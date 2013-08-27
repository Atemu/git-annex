{- Checks system configuration and generates SysConfig.hs. -}

module Build.Configure where

import System.Directory
import Data.List
import System.Process
import Control.Applicative
import System.FilePath
import System.Environment
import Data.Maybe
import Control.Monad.IfElse
import Data.Char

import Build.TestConfig
import Utility.SafeCommand
import Utility.Monad
import Utility.Exception
import Utility.ExternalSHA
import qualified Git.Version

tests :: [TestCase]
tests =
	[ TestCase "version" getVersion
	, TestCase "git" $ requireCmd "git" "git --version >/dev/null"
	, TestCase "git version" getGitVersion
	, testCp "cp_a" "-a"
	, testCp "cp_p" "-p"
	, testCp "cp_reflink_auto" "--reflink=auto"
	, TestCase "xargs -0" $ requireCmd "xargs_0" "xargs -0 </dev/null"
	, TestCase "rsync" $ requireCmd "rsync" "rsync --version >/dev/null"
	, TestCase "curl" $ testCmd "curl" "curl --version >/dev/null"
	, TestCase "wget" $ testCmd "wget" "wget --version >/dev/null"
	, TestCase "bup" $ testCmd "bup" "bup --version >/dev/null"
	, TestCase "ionice" $ testCmd "ionice" "ionice -c3 true >/dev/null"
	, TestCase "gpg" $ maybeSelectCmd "gpg"
		[ ("gpg", "--version >/dev/null")
		, ("gpg2", "--version >/dev/null") ]
	, TestCase "lsof" $ findCmdPath "lsof" "lsof"
	, TestCase "ssh connection caching" getSshConnectionCaching
	] ++ shaTestCases
	[ (1, "da39a3ee5e6b4b0d3255bfef95601890afd80709")
	, (256, "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
	, (512, "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e")
	, (224, "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f")
	, (384, "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b")
	]

{- shaNsum are the program names used by coreutils. Some systems like OSX
 - sometimes install these with 'g' prefixes.
 -
 - On some systems, shaN is used instead, but on other
 - systems, it might be "hashalot", which does not produce
 - usable checksums. Only accept programs that produce
 - known-good hashes when run on files. -}
shaTestCases :: [(Int, String)] -> [TestCase]
shaTestCases l = map make l
  where
	make (n, knowngood) = TestCase key $ 
		Config key . MaybeStringConfig <$> search (shacmds n)
	  where
		key = "sha" ++ show n
	  	search [] = return Nothing
		search (c:cmds) = do
			sha <- externalSHA c n "/dev/null"
			if sha == Right knowngood
				then return $ Just c
				else search cmds
	
	shacmds n = concatMap (\x -> [x, 'g':x, osxpath </> x]) $
		map (\x -> "sha" ++ show n ++ x) ["sum", ""]

	{- Max OSX sometimes puts GNU tools outside PATH, so look in
	 - the location it uses, and remember where to run them
	 - from. -}
	osxpath = "/opt/local/libexec/gnubin"

tmpDir :: String
tmpDir = "tmp"

testFile :: String
testFile = tmpDir ++ "/testfile"

testCp :: ConfigKey -> String -> TestCase
testCp k option = TestCase cmd $ testCmd k cmdline
  where
	cmd = "cp " ++ option
	cmdline = cmd ++ " " ++ testFile ++ " " ++ testFile ++ ".new"

isReleaseBuild :: IO Bool
isReleaseBuild = isJust <$> catchMaybeIO (getEnv "RELEASE_BUILD")

{- Version is usually based on the major version from the changelog, 
 - plus the date of the last commit, plus the git rev of that commit.
 - This works for autobuilds, ad-hoc builds, etc.
 -
 - If git or a git repo is not available, or something goes wrong,
 - or this is a release build, just use the version from the changelog. -}
getVersion :: Test
getVersion = do
	changelogversion <- getChangelogVersion
	version <- ifM (isReleaseBuild)
		( return changelogversion
		, catchDefaultIO changelogversion $ do
			let major = takeWhile (/= '.') changelogversion
			autoversion <- readProcess "sh"
				[ "-c"
				, "git log -n 1 --format=format:'%ci %h'| sed -e 's/-//g' -e 's/ .* /-g/'"
				] ""
			if null autoversion
				then return changelogversion
				else return $ concat [ major, ".", autoversion ]
		)
	return $ Config "packageversion" (StringConfig version)
	
getChangelogVersion :: IO String
getChangelogVersion = do
	changelog <- readFile "debian/changelog"
	let verline = takeWhile (/= '\n') changelog
	return $ middle (words verline !! 1)
  where
	middle = drop 1 . init

getGitVersion :: Test
getGitVersion = Config "gitversion" . StringConfig . show
	<$> Git.Version.installed

getSshConnectionCaching :: Test
getSshConnectionCaching = Config "sshconnectioncaching" . BoolConfig <$>
	boolSystem "sh" [Param "-c", Param "ssh -o ControlPersist=yes -V >/dev/null 2>/dev/null"]

{- Set up cabal file with version. -}
cabalSetup :: IO ()
cabalSetup = do
	version <- takeWhile (\c -> isDigit c || c == '.')
		<$> getChangelogVersion
	cabal <- readFile cabalfile
	writeFile tmpcabalfile $ unlines $ 
		map (setfield "Version" version) $
		lines cabal
	renameFile tmpcabalfile cabalfile
  where
	cabalfile = "git-annex.cabal"
	tmpcabalfile = cabalfile++".tmp"
	setfield field value s
		| fullfield `isPrefixOf` s = fullfield ++ value
		| otherwise = s
	  where
		fullfield = field ++ ": "

setup :: IO ()
setup = do
	createDirectoryIfMissing True tmpDir
	writeFile testFile "test file contents"

cleanup :: IO ()
cleanup = removeDirectoryRecursive tmpDir

run :: [TestCase] -> IO ()
run ts = do
	args <- getArgs
	setup
	config <- runTests ts
	if args == ["Android"]
		then writeSysConfig $ androidConfig config
		else writeSysConfig config
	cleanup
	whenM (isReleaseBuild) $
		cabalSetup

{- Hard codes some settings to cross-compile for Android. -}
androidConfig :: [Config] -> [Config]
androidConfig c = overrides ++ filter (not . overridden) c
  where
	overrides = 
		[ Config "cp_reflink_auto" $ BoolConfig False
		, Config "curl" $ BoolConfig False
		, Config "sha224" $ MaybeStringConfig Nothing
		, Config "sha384" $ MaybeStringConfig Nothing
		]
	overridden (Config k _) = k `elem` overridekeys
	overridekeys = map (\(Config k _) -> k) overrides

