{- Package version determination, for configure script. -}

module Build.Version where

import Data.Maybe
import Control.Applicative
import Data.List
import System.Environment
import System.Directory
import Data.Char
import System.Process

import Utility.Monad
import Utility.Exception

type Version = String

{- Set when making an official release. (Distribution vendors should set
 - this too.) -}
isReleaseBuild :: IO Bool
isReleaseBuild = isJust <$> catchMaybeIO (getEnv "RELEASE_BUILD")

{- Version is usually based on the major version from the changelog, 
 - plus the date of the last commit, plus the git rev of that commit.
 - This works for autobuilds, ad-hoc builds, etc.
 -
 - If git or a git repo is not available, or something goes wrong,
 - or this is a release build, just use the version from the changelog. -}
getVersion :: IO Version
getVersion = do
	changelogversion <- getChangelogVersion
	ifM (isReleaseBuild)
		( return changelogversion
		, catchDefaultIO changelogversion $ do
			let major = takeWhile (/= '.') changelogversion
			autoversion <- takeWhile (\c -> isAlphaNum c || c == '-') <$> readProcess "sh"
				[ "-c"
				, "git log -n 1 --format=format:'%ci %h'| sed -e 's/-//g' -e 's/ .* /-g/'"
				] ""
			if null autoversion
				then return changelogversion
				else return $ concat [ major, ".", autoversion ]
		)
	
getChangelogVersion :: IO Version
getChangelogVersion = do
	changelog <- readFile "debian/changelog"
	let verline = takeWhile (/= '\n') changelog
	return $ middle (words verline !! 1)
  where
	middle = drop 1 . init

{- Set up cabal file with version. -}
cabalSetup :: FilePath -> IO ()
cabalSetup cabalfile = do
	version <- takeWhile (\c -> isDigit c || c == '.')
		<$> getChangelogVersion
	cabal <- readFile cabalfile
	writeFile tmpcabalfile $ unlines $ 
		map (setfield "Version" version) $
		lines cabal
	renameFile tmpcabalfile cabalfile
  where
	tmpcabalfile = cabalfile++".tmp"
	setfield field value s
		| fullfield `isPrefixOf` s = fullfield ++ value
		| otherwise = s
	  where
		fullfield = field ++ ": "
