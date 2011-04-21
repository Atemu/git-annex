{- git-annex crypto
 -
 - Currently using gpg; could later be modified to support different
 - crypto backends if neccessary.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Crypto (
	Cipher,
	EncryptedCipher,
	genCipher,
	updateCipher,
	describeCipher,
	storeCipher,
	extractCipher,
	decryptCipher,		
	encryptKey,
	withEncryptedHandle,
	withDecryptedHandle,
	withEncryptedContent,
	withDecryptedContent,
) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import qualified Codec.Binary.Base64 as B64
import Data.String.Utils
import Data.List
import Data.Bits.Utils
import Data.HMAC
import Data.Array
import Codec.Utils
import System.Cmd.Utils
import System.IO
import System.Posix.IO
import System.Posix.Types
import System.Posix.Process
import System.Exit
import System.Environment
import Control.Concurrent
import Control.Exception (finally)

import Types
import Key
import RemoteClass
import Utility
import CryptoTypes

{- The first half of a Cipher is used for HMAC; the remainder
 - is used as the GPG symmetric encryption passphrase.
 -
 - HMAC SHA1 needs only 64 bytes. The remainder is for expansion,
 - perhaps to HMAC SHA512, which needs 128 bytes (ideally).
 -
 - 256 is enough for gpg's symetric cipher; unlike weaker public key
 - crypto, the key does not need to be too large.
 -}
cipherHalf :: Int
cipherHalf = 256

cipherSize :: Int
cipherSize = cipherHalf * 2

cipherPassphrase :: Cipher -> String
cipherPassphrase (Cipher c) = drop cipherHalf c

cipherHmac :: Cipher -> String
cipherHmac (Cipher c) = take cipherHalf c

{- Creates a new Cipher, encrypted as specified in the remote's configuration -}
genCipher :: RemoteConfig -> IO EncryptedCipher
genCipher c = do
	ks <- configKeyIds c
	random <- genrandom
	encryptCipher (Cipher random) ks
	where
		genrandom = gpgRead
			-- Armor the random data, to avoid newlines,
			-- since gpg only reads ciphers up to the first
			-- newline.
			[ Params "--gen-random --armor"
			, Param $ show randomquality
			, Param $ show cipherSize
			]
		-- 1 is /dev/urandom; 2 is /dev/random
		randomquality = 1 :: Int

{- Updates an existing Cipher, re-encrypting it to add KeyIds specified in
 - the remote's configuration. -}
updateCipher :: RemoteConfig -> EncryptedCipher -> IO EncryptedCipher
updateCipher c encipher@(EncryptedCipher _ ks) = do
	ks' <- configKeyIds c
	cipher <- decryptCipher c encipher
	encryptCipher cipher (combine ks ks')
	where
		combine (KeyIds a) (KeyIds b) = KeyIds $ a ++ b

describeCipher :: EncryptedCipher -> String
describeCipher (EncryptedCipher _ (KeyIds ks)) =
	"with gpg " ++ keys ks ++ " " ++ unwords ks
	where
		keys [_] = "key"
		keys _ = "keys"

{- Stores an EncryptedCipher in a remote's configuration. -}
storeCipher :: RemoteConfig -> EncryptedCipher -> RemoteConfig
storeCipher c (EncryptedCipher t ks) = 
	M.insert "cipher" (toB64 t) $ M.insert "cipherkeys" (show ks) c

{- Extracts an EncryptedCipher from a remote's configuration. -}
extractCipher :: RemoteConfig -> Maybe EncryptedCipher
extractCipher c = 
	case (M.lookup "cipher" c, M.lookup "cipherkeys" c) of
		(Just t, Just ks) -> Just $ EncryptedCipher (fromB64 t) (read ks)
		_ -> Nothing

{- Encrypts a Cipher to the specified KeyIds. -}
encryptCipher :: Cipher -> KeyIds -> IO EncryptedCipher
encryptCipher (Cipher c) (KeyIds ks) = do
	let ks' = nub $ sort ks -- gpg complains about duplicate recipient keyids
	encipher <- gpgPipeStrict (encrypt++recipients ks') c
	return $ EncryptedCipher encipher (KeyIds ks')
	where
		encrypt = [ Params "--encrypt" ]
		recipients l = 
			-- Force gpg to only encrypt to the specified
			-- recipients, not configured defaults.
			[ Params "--no-encrypt-to --no-default-recipient"] ++
			(concat $ map (\k -> [Param "--recipient", Param k]) l)

{- Decrypting an EncryptedCipher is expensive; the Cipher should be cached. -}
decryptCipher :: RemoteConfig -> EncryptedCipher -> IO Cipher
decryptCipher _ (EncryptedCipher encipher _) = 
	return . Cipher =<< gpgPipeStrict decrypt encipher
	where
		decrypt = [ Param "--decrypt" ]

{- Generates an encrypted form of a Key. The encryption does not need to be
 - reversable, nor does it need to be the same type of encryption used
 - on content. It does need to be repeatable. -}
encryptKey :: Cipher -> Key -> IO Key
encryptKey c k =
	return Key {
		keyName = showOctets $ hmac_sha1
			(s2w8 $ cipherHmac c)
			(s2w8 $ show k),
		keyBackendName = "GPGHMACSHA1",
		keySize = Nothing, -- size and mtime omitted
		keyMtime = Nothing -- to avoid leaking data
	}

{- Runs an action, passing it a handle from which it can 
 - stream encrypted content. -}
withEncryptedHandle :: Cipher -> (IO L.ByteString) -> (Handle -> IO a) -> IO a
withEncryptedHandle = gpgCipherHandle [Params "--symmetric --force-mdc"]

{- Runs an action, passing it a handle from which it can
 - stream decrypted content. -}
withDecryptedHandle :: Cipher -> (IO L.ByteString) -> (Handle -> IO a) -> IO a
withDecryptedHandle = gpgCipherHandle [Param "--decrypt"]

{- Streams encrypted content to an action. -}
withEncryptedContent :: Cipher -> (IO L.ByteString) -> (L.ByteString -> IO a) -> IO a
withEncryptedContent = pass withEncryptedHandle

{- Streams decrypted content to an action. -}
withDecryptedContent :: Cipher -> (IO L.ByteString) -> (L.ByteString -> IO a) -> IO a
withDecryptedContent = pass withDecryptedHandle

pass :: (Cipher -> (IO L.ByteString) -> (Handle -> IO a) -> IO a) 
      -> Cipher -> (IO L.ByteString) -> (L.ByteString -> IO a) -> IO a
pass to c i a = to c i $ \h -> a =<< L.hGetContents h

gpgParams :: [CommandParam] -> IO [String]
gpgParams params = do
	-- Enable batch mode if GPG_AGENT_INFO is set, to avoid extraneous
	-- gpg output about password prompts.
	e <- catch (getEnv "GPG_AGENT_INFO") (const $ return "")
	let batch = if null e then [] else ["--batch"]
	return $ batch ++ defaults ++ toCommand params
	where
		-- be quiet, even about checking the trustdb
		defaults = ["--quiet", "--trust-model", "always"]

gpgRead :: [CommandParam] -> IO String
gpgRead params = do
	params' <- gpgParams params
	pOpen ReadFromPipe "gpg" params' hGetContentsStrict

gpgPipeStrict :: [CommandParam] -> String -> IO String
gpgPipeStrict params input = do
	params' <- gpgParams params
	(pid, fromh, toh) <- hPipeBoth "gpg" params'
	_ <- forkIO $ finally (hPutStr toh input) (hClose toh)
	output <- hGetContentsStrict fromh
	forceSuccess pid
	return output

{- Runs gpg with a cipher and some parameters, feeding it an input,
 - and passing a handle to its output to an action.
 -
 - Note that to avoid deadlock with the cleanup stage,
 - the action must fully consume gpg's input before returning. -}
gpgCipherHandle :: [CommandParam] -> Cipher -> (IO L.ByteString) -> (Handle -> IO a) -> IO a
gpgCipherHandle params c a b = do
	-- pipe the passphrase into gpg on a fd
	(frompipe, topipe) <- createPipe
	_ <- forkIO $ do
		toh <- fdToHandle topipe
		hPutStrLn toh $ cipherPassphrase c
		hClose toh
	let Fd passphrasefd = frompipe
	let passphrase = [Param "--passphrase-fd", Param $ show passphrasefd]

	params' <- gpgParams $ passphrase ++ params
	(pid, fromh, toh) <- hPipeBoth "gpg" params'
	_ <- forkProcess $ do
		L.hPut toh =<< a
		hClose toh
		exitSuccess
	hClose toh
	ret <- b fromh

	-- cleanup
	forceSuccess pid
	closeFd frompipe
	return ret

configKeyIds :: RemoteConfig -> IO KeyIds
configKeyIds c = do
	let k = configGet c "encryption"
	s <- gpgRead [Params "--with-colons --list-public-keys", Param k]
	return $ KeyIds $ parseWithColons s
	where
		parseWithColons s = map keyIdField $ filter pubKey $ lines s
		pubKey = isPrefixOf "pub:"
		keyIdField s = (split ":" s) !! 4

configGet :: RemoteConfig -> String -> String
configGet c key =
	case M.lookup key c of
		Just v -> v
		Nothing -> error $ "missing " ++ key ++ " in remote config"

toB64 :: String -> String		
toB64 = B64.encode . s2w8

fromB64 :: String -> String
fromB64 s =
	case B64.decode s of
		Nothing -> error "bad base64 encoded data"
		Just ws -> w82s ws

showOctets :: [Octet] -> String
showOctets = concat . map hexChars
	where
		hexChars c = [arr ! (c `div` 16), arr ! (c `mod` 16)]
		arr = listArray (0, 15) "0123456789abcdef"

