{- Linux library copier and binary shimmer
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Main where

import Control.Applicative
import System.Environment
import Data.Maybe
import System.FilePath
import System.Directory
import Control.Monad
import Data.List
import System.Posix.Files
import Control.Monad.IfElse

import Utility.LinuxMkLibs
import Utility.Directory
import Utility.Process
import Utility.Monad
import Utility.Path
import Utility.FileMode
import Utility.CopyFile

main :: IO ()
main = getArgs >>= go
  where
	go [] = error "specify LINUXSTANDALONE_DIST"
	go (top:_) = mklibs top

mklibs :: FilePath -> IO ()
mklibs top = do
	fs <- dirContentsRecursive top
	mapM_ symToHardLink fs
	exes <- filterM checkExe fs
	libs <- parseLdd <$> readProcess "ldd" exes
	glibclibs <- glibcLibs
	let libs' = nub $ libs ++ glibclibs
	libdirs <- nub . catMaybes <$> mapM (installLib installFile top) libs'

	-- Various files used by runshell to set up env vars used by the
	-- linker shims.
	writeFile (top </> "libdirs") (unlines libdirs)
	writeFile (top </> "linker")
		(Prelude.head $ filter ("ld-linux" `isInfixOf`) libs')
	writeFile (top </> "gconvdir")
		(parentDir $ Prelude.head $ filter ("/gconv/" `isInfixOf`) glibclibs)
	
	mapM_ (installLinkerShim top) exes

{- Installs a linker shim script around a binary.
 -
 - Note that each binary is put into its own separate directory,
 - to avoid eg git looking for binaries in its directory rather
 - than in PATH.-}
installLinkerShim :: FilePath -> FilePath -> IO ()
installLinkerShim top exe = do
	createDirectoryIfMissing True shimdir
	renameFile exe exedest
	writeFile exe $ unlines
		[ "#!/bin/sh"
		, "exec \"$GIT_ANNEX_LINKER\" --library-path \"$GIT_ANNEX_LD_LIBRARY_PATH\" \"$GIT_ANNEX_SHIMMED/" ++ base ++ "/" ++ base ++ "\" \"$@\""
		]
	modifyFileMode exe $ addModes executeModes
  where
	base = takeFileName exe
	shimdir = top </> "shimmed" </> base
	exedest = shimdir </> base

{- Converting symlinks to hard links simplifies the binary shimming
 - process. -}
symToHardLink :: FilePath -> IO ()
symToHardLink f = whenM (isSymbolicLink <$> getSymbolicLinkStatus f) $ do
	l <- readSymbolicLink f
	let absl = absPathFrom (parentDir f) l
	nukeFile f
	createLink absl f

installFile :: FilePath -> FilePath -> IO ()
installFile top f = do
	createDirectoryIfMissing True destdir
	void $ copyFileExternal CopyTimeStamps f destdir
  where
	destdir = inTop top $ parentDir f

checkExe :: FilePath -> IO Bool
checkExe f
	| ".so" `isSuffixOf` f = return False
	| otherwise = ifM (isExecutable . fileMode <$> getFileStatus f)
		( checkFileExe <$> readProcess "file" [f]
		, return False
		)

{- Check that file(1) thinks it's a Linux ELF executable, or possibly
 - a shared library (a few executables like ssh appear as shared libraries). -}
checkFileExe :: String -> Bool
checkFileExe s = and
	[ "ELF" `isInfixOf` s
	, "executable" `isInfixOf` s || "shared object" `isInfixOf` s
	]
