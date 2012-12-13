{- git-annex crypto
 -
 - Currently using gpg; could later be modified to support different
 - crypto backends if neccessary.
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Crypto (
	Cipher,
	KeyIds(..),
	StorableCipher(..),
	genEncryptedCipher,
	genSharedCipher,
	updateEncryptedCipher,
	describeCipher,
	decryptCipher,		
	encryptKey,
	feedFile,
	feedBytes,
	readBytes,
	encrypt,
	decrypt,	

	prop_hmacWithCipher_sane
) where

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.SHA
import Control.Applicative

import Common.Annex
import qualified Utility.Gpg as Gpg
import Types.Key
import Types.Crypto

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

{- Creates a new Cipher, encrypted to the specificed key id. -}
genEncryptedCipher :: String -> IO StorableCipher
genEncryptedCipher keyid = do
	ks <- Gpg.findPubKeys keyid
	random <- Gpg.genRandom cipherSize
	encryptCipher (Cipher random) ks

{- Creates a new, shared Cipher. -}
genSharedCipher :: IO StorableCipher
genSharedCipher = SharedCipher <$> Gpg.genRandom cipherSize

{- Updates an existing Cipher, re-encrypting it to add a keyid. -}
updateEncryptedCipher :: String -> StorableCipher -> IO StorableCipher
updateEncryptedCipher _ (SharedCipher _) = undefined
updateEncryptedCipher keyid encipher@(EncryptedCipher _ ks) = do
	ks' <- Gpg.findPubKeys keyid
	cipher <- decryptCipher encipher
	encryptCipher cipher (merge ks ks')
  where
	merge (KeyIds a) (KeyIds b) = KeyIds $ a ++ b

describeCipher :: StorableCipher -> String
describeCipher (SharedCipher _) = "shared cipher"
describeCipher (EncryptedCipher _ (KeyIds ks)) =
	"with gpg " ++ keys ks ++ " " ++ unwords ks
  where
	keys [_] = "key"
	keys _ = "keys"

{- Encrypts a Cipher to the specified KeyIds. -}
encryptCipher :: Cipher -> KeyIds -> IO StorableCipher
encryptCipher (Cipher c) (KeyIds ks) = do
	let ks' = nub $ sort ks -- gpg complains about duplicate recipient keyids
	encipher <- Gpg.pipeStrict ([ Params "--encrypt" ] ++ recipients ks') c
	return $ EncryptedCipher encipher (KeyIds ks')
  where
	recipients l = force_recipients :
		concatMap (\k -> [Param "--recipient", Param k]) l
	-- Force gpg to only encrypt to the specified
	-- recipients, not configured defaults.
	force_recipients = Params "--no-encrypt-to --no-default-recipient"

{- Decrypting an EncryptedCipher is expensive; the Cipher should be cached. -}
decryptCipher :: StorableCipher -> IO Cipher
decryptCipher (SharedCipher t) = return $ Cipher t
decryptCipher (EncryptedCipher t _) = Cipher <$> Gpg.pipeStrict [ Param "--decrypt" ] t

{- Generates an encrypted form of a Key. The encryption does not need to be
 - reversable, nor does it need to be the same type of encryption used
 - on content. It does need to be repeatable. -}
encryptKey :: Cipher -> Key -> Key
encryptKey c k = Key
	{ keyName = hmacWithCipher c (key2file k)
	, keyBackendName = "GPGHMACSHA1"
	, keySize = Nothing -- size and mtime omitted
	, keyMtime = Nothing -- to avoid leaking data
	}

type Feeder = Handle -> IO ()
type Reader a = Handle -> IO a

feedFile :: FilePath -> Feeder
feedFile f h = L.hPut h =<< L.readFile f

feedBytes :: L.ByteString -> Feeder
feedBytes = flip L.hPut

readBytes :: (L.ByteString -> IO a) -> Reader a
readBytes a h = L.hGetContents h >>= a

{- Runs a Feeder action, that generates content that is encrypted with the
 - Cipher, and read by the Reader action. -}
encrypt :: Cipher -> Feeder -> Reader a -> IO a
encrypt = Gpg.feedRead [Params "--symmetric --force-mdc"] . cipherPassphrase

{- Runs a Feeder action, that generates content that is decrypted with the
 - Cipher, and read by the Reader action. -}
decrypt :: Cipher -> Feeder -> Reader a -> IO a
decrypt = Gpg.feedRead [Param "--decrypt"] . cipherPassphrase

hmacWithCipher :: Cipher -> String -> String
hmacWithCipher c = hmacWithCipher' (cipherHmac c) 
hmacWithCipher' :: String -> String -> String
hmacWithCipher' c s = showDigest $ hmacSha1 (fromString c) (fromString s)

{- Ensure that hmacWithCipher' returns the same thing forevermore. -}
prop_hmacWithCipher_sane :: Bool
prop_hmacWithCipher_sane = known_good == hmacWithCipher' "foo" "bar"
  where
	known_good = "46b4ec586117154dacd49d664e5d63fdc88efb51"
