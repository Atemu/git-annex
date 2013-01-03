{- git-annex assistant ssh utilities
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Ssh where

import Common.Annex
import Utility.TempFile
import Utility.UserInfo
import Git.Remote

import Data.Text (Text)
import qualified Data.Text as T
import qualified Control.Exception as E
import System.Process (CreateProcess(..))
import Control.Concurrent
import Data.Char

data SshData = SshData
	{ sshHostName :: Text
	, sshUserName :: Maybe Text
	, sshDirectory :: Text
	, sshRepoName :: String
	, sshPort :: Int
	, needsPubKey :: Bool
	, rsyncOnly :: Bool
	}
	deriving (Read, Show, Eq)

data SshKeyPair = SshKeyPair
	{ sshPubKey :: String
	, sshPrivKey :: String
	}

instance Show SshKeyPair where
	show = sshPubKey

type SshPubKey = String

{- ssh -ofoo=bar command-line option -}
sshOpt :: String -> String -> String
sshOpt k v = concat ["-o", k, "=", v]

sshDir :: IO FilePath
sshDir = do
	home <- myHomeDir
	return $ home </> ".ssh"

{- user@host or host -}
genSshHost :: Text -> Maybe Text -> String
genSshHost host user = maybe "" (\v -> T.unpack v ++ "@") user ++ T.unpack host

{- Generates a git remote name, like host_dir or host -}
genSshRepoName :: String -> FilePath -> String
genSshRepoName host dir
	| null dir = makeLegalName host
	| otherwise = makeLegalName $ host ++ "_" ++ dir

{- The output of ssh, including both stdout and stderr. -}
sshTranscript :: [String] -> String -> IO (String, Bool)
sshTranscript opts input = do
	(readf, writef) <- createPipe
	readh <- fdToHandle readf
	writeh <- fdToHandle writef
	(Just inh, _, _, pid) <- createProcess $
		(proc "ssh" opts)
			{ std_in = CreatePipe
			, std_out = UseHandle writeh
			, std_err = UseHandle writeh
			}
	hClose writeh

	-- fork off a thread to start consuming the output
	transcript <- hGetContents readh
	outMVar <- newEmptyMVar
	_ <- forkIO $ E.evaluate (length transcript) >> putMVar outMVar ()

	-- now write and flush any input
	unless (null input) $ do
		hPutStr inh input
		hFlush inh
	hClose inh -- done with stdin

	-- wait on the output
	takeMVar outMVar
	hClose readh

	ok <- checkSuccessProcess pid
	return (transcript, ok)

{- Ensure that the ssh public key doesn't include any ssh options, like
 - command=foo, or other weirdness -}
validateSshPubKey :: SshPubKey -> IO ()
validateSshPubKey pubkey = either error return $ check $ words pubkey
  where
	check [prefix, _key, comment] = do
		checkprefix prefix
		checkcomment comment
	check [prefix, _key] =
		checkprefix prefix
	check _ = err "wrong number of words in ssh public key"

	ok = Right ()
	err msg = Left $ unwords [msg, pubkey]

	checkprefix prefix
		| ssh == "ssh" && all isAlphaNum keytype = ok
		| otherwise = err "bad ssh public key prefix"
	  where
		(ssh, keytype) = separate (== '-') prefix

	checkcomment comment
		| all (\c -> isAlphaNum c || c == '@' || c == '-' || c == '_' || c == '.') comment = ok
		| otherwise = err "bad comment in ssh public key"

addAuthorizedKeys :: Bool -> FilePath -> SshPubKey -> IO Bool
addAuthorizedKeys rsynconly dir pubkey = boolSystem "sh"
	[ Param "-c" , Param $ addAuthorizedKeysCommand rsynconly dir pubkey ]

removeAuthorizedKeys :: Bool -> FilePath -> SshPubKey -> IO ()
removeAuthorizedKeys rsynconly dir pubkey = do
	let keyline = authorizedKeysLine rsynconly dir pubkey
	sshdir <- sshDir
	let keyfile = sshdir </> "authorized_keys"
	ls <- lines <$> readFileStrict keyfile
	writeFile keyfile $ unlines $ filter (/= keyline) ls

{- Implemented as a shell command, so it can be run on remote servers over
 - ssh.
 -
 - The ~/.ssh/git-annex-shell wrapper script is created if not already
 - present.
 -}
addAuthorizedKeysCommand :: Bool -> FilePath -> SshPubKey -> String
addAuthorizedKeysCommand rsynconly dir pubkey = join "&&"
	[ "mkdir -p ~/.ssh"
	, join "; "
		[ "if [ ! -e " ++ wrapper ++ " ]"
		, "then (" ++ join ";" (map echoval script) ++ ") > " ++ wrapper
		, "fi"
		]
	, "chmod 700 " ++ wrapper
	, "touch ~/.ssh/authorized_keys"
	, "chmod 600 ~/.ssh/authorized_keys"
	, unwords
		[ "echo"
		, shellEscape $ authorizedKeysLine rsynconly dir pubkey
		, ">>~/.ssh/authorized_keys"
		]
	]
  where
	echoval v = "echo " ++ shellEscape v
	wrapper = "~/.ssh/git-annex-shell"
	script =
		[ "#!/bin/sh"
		, "set -e"
		, "exec git-annex-shell -c \"$SSH_ORIGINAL_COMMAND\""
		]

authorizedKeysLine :: Bool -> FilePath -> SshPubKey -> String
authorizedKeysLine rsynconly dir pubkey
	{- TODO: Locking down rsync is difficult, requiring a rather
	 - long perl script. -}
	| rsynconly = pubkey
	| otherwise = limitcommand ++ pubkey
  where
	limitcommand = "command=\"GIT_ANNEX_SHELL_DIRECTORY="++shellEscape dir++" ~/.ssh/git-annex-shell\",no-agent-forwarding,no-port-forwarding,no-X11-forwarding "

{- Generates a ssh key pair. -}
genSshKeyPair :: IO SshKeyPair
genSshKeyPair = withTempDir "git-annex-keygen" $ \dir -> do
	ok <- boolSystem "ssh-keygen"
		[ Param "-P", Param "" -- no password
		, Param "-f", File $ dir </> "key"
		]
	unless ok $
		error "ssh-keygen failed"
	SshKeyPair
		<$> readFile (dir </> "key.pub")
		<*> readFile (dir </> "key")

{- Installs a ssh key pair, and sets up ssh config with a mangled hostname
 - that will enable use of the key. This way we avoid changing the user's
 - regular ssh experience at all. Returns a modified SshData containing the
 - mangled hostname. -}
setupSshKeyPair :: SshKeyPair -> SshData -> IO SshData
setupSshKeyPair sshkeypair sshdata = do
	sshdir <- sshDir
	createDirectoryIfMissing True sshdir

	unlessM (doesFileExist $ sshdir </> sshprivkeyfile) $ do
		h <- fdToHandle =<<
			createFile (sshdir </> sshprivkeyfile)
				(unionFileModes ownerWriteMode ownerReadMode)
		hPutStr h (sshPrivKey sshkeypair)
		hClose h
	unlessM (doesFileExist $ sshdir </> sshpubkeyfile) $
		writeFile (sshdir </> sshpubkeyfile) (sshPubKey sshkeypair)

	setSshConfig sshdata
		[ ("IdentityFile", "~/.ssh/" ++ sshprivkeyfile) ]
  where
	sshprivkeyfile = "key." ++ mangleSshHostName sshdata
	sshpubkeyfile = sshprivkeyfile ++ ".pub"

{- Setups up a ssh config with a mangled hostname.
 - Returns a modified SshData containing the mangled hostname. -}
setSshConfig :: SshData -> [(String, String)] -> IO SshData
setSshConfig sshdata config = do
	sshdir <- sshDir
	createDirectoryIfMissing True sshdir
	let configfile = sshdir </> "config"
	unlessM (catchBoolIO $ isInfixOf mangledhost <$> readFile configfile) $
		appendFile configfile $ unlines $
			[ ""
			, "# Added automatically by git-annex"
			, "Host " ++ mangledhost
			] ++ map (\(k, v) -> "\t" ++ k ++ " " ++ v)
				(settings ++ config)
	return $ sshdata { sshHostName = T.pack mangledhost }
  where
	mangledhost = mangleSshHostName sshdata
	settings =
		[ ("Hostname", T.unpack $ sshHostName sshdata)
		, ("Port", show $ sshPort sshdata)
		]

mangleSshHostName :: SshData -> String
mangleSshHostName sshdata = "git-annex-" ++ host ++ (maybe "-" ('-':) user)
  where
	host = T.unpack $ sshHostName sshdata
	user = T.unpack <$> sshUserName sshdata

unMangleSshHostName :: String -> String
unMangleSshHostName h
	| "git-annex-" `isPrefixOf` h = join "-" (beginning $ drop 2 dashbits)
	| otherwise = h
  where
	dashbits = split "-" h

{- Does ssh have known_hosts data for a hostname? -}
knownHost :: Text -> IO Bool
knownHost hostname = do
	sshdir <- sshDir
	ifM (doesFileExist $ sshdir </> "known_hosts")
		( not . null <$> checkhost
		, return False
		)
  where
	{- ssh-keygen -F can crash on some old known_hosts file -}
	checkhost = catchDefaultIO "" $
		readProcess "ssh-keygen" ["-F", T.unpack hostname]
