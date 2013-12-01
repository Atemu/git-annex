{- Bundled programs
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Build.BundledPrograms where

import Data.Maybe

import Build.SysConfig as SysConfig

{- Programs that git-annex uses, to include in the bundle.
 -
 - These may be just the command name, or the full path to it. -}
bundledPrograms :: [FilePath]
bundledPrograms = catMaybes
	[ Nothing
#ifndef mingw32_HOST_OS
	-- git is not included in the windows bundle
	, Just "git"
#endif
	, Just "cp"
#ifndef mingw32_HOST_OS
	-- using xargs on windows led to problems, so it's not used there
	, Just "xargs"
#endif
	, Just "rsync"
	, Just "ssh"
	, Just "ssh-keygen"
#ifndef mingw32_HOST_OS
	, Just "sh"
#endif
	, SysConfig.gpg
	, ifset SysConfig.curl "curl"
	, ifset SysConfig.wget "wget"
	, ifset SysConfig.bup "bup"
	, SysConfig.lsof
	, SysConfig.gcrypt
	, SysConfig.sha1
	, SysConfig.sha256
	, SysConfig.sha512
	, SysConfig.sha224
	, SysConfig.sha384
#ifdef linux_HOST_OS
	-- used to unpack the tarball when upgrading
	, Just "gunzip"
	, Just "tar"
#endif
	-- nice, ionice, and nocache are not included in the bundle;
	-- we rely on the system's own version, which may better match
	-- its kernel, and avoid using them if not available.
	]
  where
	ifset True s = Just s
	ifset False _ = Nothing
