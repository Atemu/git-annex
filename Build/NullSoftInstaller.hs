{- Generates a NullSoft installer program for git-annex on Windows.
 - 
 - To build the installer, git-annex should already be built by cabal,
 - and ssh and rsync, as well as cygwin libraries, already installed.
 -
 - This uses the Haskell nsis package (cabal install nsis)
 - to generate a .nsi file, which is then used to produce
 - git-annex-installer.exe
 - 
 - The installer includes git-annex, and utilities it uses, with the
 - exception of git. The user needs to install git separately,
 - and the installer checks for that.
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

import Development.NSIS
import System.Directory
import System.FilePath
import Control.Monad
import Data.String
import Data.Maybe

import Utility.Tmp
import Utility.Path
import Utility.CopyFile
import Utility.SafeCommand
import Build.BundledPrograms

main = do
	withTmpDir "nsis-build" $ \tmpdir -> do
		let gitannex = tmpdir </> gitannexprogram
		mustSucceed "ln" [File "dist/build/git-annex/git-annex.exe", File gitannex]
		let license = tmpdir </> licensefile
		mustSucceed "sh" [Param "-c", Param $ "zcat standalone/licences.gz > '" ++ license ++ "'"]
		extrabins <- forM (cygwinPrograms ++ cygwinDlls) $ \f -> do
			p <- searchPath f
			when (isNothing p) $
				print ("unable to find in PATH", f)
			return p
		webappscript <- vbsLauncher tmpdir "git-annex-webapp" "git-annex webapp"
		autostartscript <- vbsLauncher tmpdir "git-annex-autostart" "git annex assistant --autostart"
		let htmlhelp = tmpdir </> "git-annex.html"
		writeFile htmlhelp htmlHelpText
		writeFile nsifile $ makeInstaller gitannex license htmlhelp
			(catMaybes extrabins)
			[ webappscript, autostartscript ]
		mustSucceed "makensis" [File nsifile]
	removeFile nsifile -- left behind if makensis fails
  where
	nsifile = "git-annex.nsi"
	mustSucceed cmd params = do
		r <- boolSystem cmd params
		case r of
			True -> return ()
			False -> error $ cmd ++ " failed"

{- Generates a .vbs launcher which runs a command without any visible DOS
 - box. -}
vbsLauncher :: FilePath -> String -> String -> IO String
vbsLauncher tmpdir basename cmd = do
	let f = tmpdir </> basename ++ ".vbs"
	writeFile f $ unlines
		[ "Set objshell=CreateObject(\"Wscript.Shell\")"
		, "objShell.Run(\"" ++ cmd ++ "\"), 0, False"
		]
	return f

gitannexprogram :: FilePath
gitannexprogram = "git-annex.exe"

licensefile :: FilePath
licensefile = "git-annex-licenses.txt"

installer :: FilePath
installer = "git-annex-installer.exe"

uninstaller :: FilePath
uninstaller = "git-annex-uninstall.exe"

gitInstallDir :: Exp FilePath
gitInstallDir = fromString "$PROGRAMFILES\\Git"

-- This intentionall has a different name than git-annex or
-- git-annex-webapp, since it is itself treated as an executable file.
-- Also, on XP, the filename is displayed, not the description.
startMenuItem :: Exp FilePath
startMenuItem = "$SMPROGRAMS/Git Annex (Webapp).lnk"

oldStartMenuItem :: Exp FilePath
oldStartMenuItem = "$SMPROGRAMS/git-annex.lnk"

autoStartItem :: Exp FilePath
autoStartItem = "$SMSTARTUP/git-annex-autostart.lnk"

needGit :: Exp String
needGit = strConcat
	[ fromString "You need git installed to use git-annex. Looking at "
	, gitInstallDir
	, fromString " , it seems to not be installed, "
	, fromString "or may be installed in another location. "
	, fromString "You can install git from http:////git-scm.com//"
	]

makeInstaller :: FilePath -> FilePath -> FilePath -> [FilePath] -> [FilePath] -> String
makeInstaller gitannex license htmlhelp extrabins launchers = nsis $ do
	name "git-annex"
	outFile $ str installer
	{- Installing into the same directory as git avoids needing to modify
	 - path myself, since the git installer already does it. -}
	installDir gitInstallDir
	requestExecutionLevel Admin

	iff (fileExists gitInstallDir)
		(return ())
		(alert needGit)
	
	-- Pages to display
	page Directory                   -- Pick where to install
	page (License license)
	page InstFiles                   -- Give a progress bar while installing
	-- Start menu shortcut
	Development.NSIS.createDirectory "$SMPROGRAMS"
	createShortcut startMenuItem
		[ Target "wscript.exe"
		, Parameters "\"$INSTDIR/git-annex-webapp.vbs\""
		, StartOptions "SW_SHOWNORMAL"
		, IconFile "$INSTDIR/cmd/git-annex.exe"
		, IconIndex 2
		, Description "Git Annex (Webapp)"
		]
	delete [RebootOK] $ oldStartMenuItem
	createShortcut autoStartItem
		[ Target "wscript.exe"
		, Parameters "\"$INSTDIR/git-annex-autostart.vbs\""
		, StartOptions "SW_SHOWNORMAL"
		, IconFile "$INSTDIR/cmd/git-annex.exe"
		, IconIndex 2
		, Description "git-annex autostart"
		]
	section "cmd" [] $ do
		setOutPath "$INSTDIR\\cmd"
		mapM_ addfile (gitannex:extrabins)
	section "meta" [] $ do
		setOutPath "$INSTDIR\\doc\\git\\html"
		addfile htmlhelp
		setOutPath "$INSTDIR"
		addfile license
		mapM_ addfile launchers
		writeUninstaller $ str uninstaller
	uninstall $ do
		delete [RebootOK] $ startMenuItem
		delete [RebootOK] $ autoStartItem
		removefilesFrom "$INSTDIR/cmd" (gitannex:extrabins)
		removefilesFrom "$INSTDIR\\doc\\git\\html" [htmlhelp]
		removefilesFrom "$INSTDIR" $
			launchers ++
			[ license
			, uninstaller
			]
  where
	addfile f = file [] (str f)
	removefilesFrom d = mapM_ (\f -> delete [RebootOK] $ fromString $ d ++ "/" ++ takeFileName f)

cygwinPrograms :: [FilePath]
cygwinPrograms = map (\p -> p ++ ".exe") bundledPrograms

-- These are the dlls needed by Cygwin's rsync, ssh, etc.
-- TODO: Use ldd (available in cygwin) to automatically find all
-- needed libs.
cygwinDlls :: [FilePath]
cygwinDlls =
	[ "cygwin1.dll"
	, "cygasn1-8.dll"
	, "cygattr-1.dll"
	, "cygheimbase-1.dll"
	, "cygroken-18.dll"
	, "cygcom_err-2.dll"
	, "cygheimntlm-0.dll"
	, "cygsqlite3-0.dll"
	, "cygcrypt-0.dll"
	, "cyghx509-5.dll"
	, "cygssp-0.dll"
	, "cygcrypto-1.0.0.dll"
	, "cygiconv-2.dll"
	, "cyggcc_s-1.dll"
	, "cygintl-8.dll"
	, "cygwind-0.dll"
	, "cyggssapi-3.dll"
	, "cyggssapi_krb5-2.dll"
	, "cygkrb5-26.dll"
	, "cygz.dll"
	, "cygidn-11.dll"
	, "cyggnutls-28.dll"
	, "libcrypto.dll"
	, "libssl.dll"
	, "cyggcrypt-11.dll"
	, "cyggpg-error-0.dll"
	, "cygp11-kit-0.dll"
	, "cygffi-6.dll"
	, "cygbz2-1.dll"
	, "cygreadline7.dll"
	, "cygncursesw-10.dll"
	, "cygusb0.dll"
	, "cyghogweed-2.dll"
	, "cygk5crypto-3.dll"
	, "cygkrb5support-0.dll"
	, "cyggmp-10.dll"
	, "cygkrb5-3.dll"
	, "cygnettle-4.dll"
	, "cygtasn1-6.dll"
	]

-- msysgit opens Program Files/Git/doc/git/html/git-annex.html
-- when git annex --help is run.
htmlHelpText :: String
htmlHelpText = unlines
	[ "<html>"
	, "<title>git-annex help</title>"
	, "<body>"
	, "For help on git-annex, run \"git annex help\", or"
	, "<a href=\"https://git-annex.branchable.com/git-annex/\">read the man page</a>."
	, "</body>"
	, "</html"
	]
