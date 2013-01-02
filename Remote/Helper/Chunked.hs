{- git-annex chunked remotes
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Chunked where

import Common.Annex
import Utility.DataUnits
import Types.Remote
import Meters

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import Data.Int
import qualified Control.Exception as E

type ChunkSize = Maybe Int64

{- Gets a remote's configured chunk size. -}
chunkSize :: RemoteConfig -> ChunkSize
chunkSize m =
	case M.lookup "chunksize" m of
		Nothing -> Nothing
		Just v -> case readSize dataUnits v of
			Nothing -> error "bad chunksize"
			Just size
				| size <= 0 -> error "bad chunksize"
				| otherwise -> Just $ fromInteger size

{- This is an extension that's added to the usual file (or whatever)
 - where the remote stores a key. -}
type ChunkExt = String

{- A record of the number of chunks used.
 -
 - While this can be guessed at based on the size of the key, encryption
 - makes that larger. Also, using this helps deal with changes to chunksize
 - over the life of a remote.
 -}
chunkCount :: ChunkExt
chunkCount = ".chunkcount"

{- Parses the String from the chunkCount file, and returns the files that
 - are used to store the chunks. -}
listChunks :: FilePath -> String -> [FilePath]
listChunks basedest chunkcount = take count $ map (basedest ++) chunkStream
  where
	count = fromMaybe 0 $ readish chunkcount

{- An infinite stream of extensions to use for chunks. -}
chunkStream :: [ChunkExt]
chunkStream = map (\n -> ".chunk" ++ show n) [1 :: Integer ..]

{- Given the base destination to use to store a value,
 - generates a stream of temporary destinations (just one when not chunking)
 - and passes it to an action, which should chunk and store the data,
 - and return the destinations it stored to, or [] on error. Then
 - calls the storer to write the chunk count (if chunking). Finally, the
 - finalizer is called to rename the tmp into the dest 
 - (and do any other cleanup).
 -}
storeChunks :: Key -> FilePath -> FilePath -> ChunkSize -> ([FilePath] -> IO [FilePath]) -> (FilePath -> String -> IO ()) -> (FilePath -> FilePath -> IO ()) -> IO Bool
storeChunks key tmp dest chunksize storer recorder finalizer = either onerr return
	=<< (E.try go :: IO (Either E.SomeException Bool))
  where
	go = do
		stored <- storer tmpdests
		when (chunksize /= Nothing) $ do
			let chunkcount = basef ++ chunkCount
			recorder chunkcount (show $ length stored)
		finalizer tmp dest
		return (not $ null stored)
	onerr e = do
		print e
		return False

	basef = tmp ++ keyFile key
	tmpdests
		| chunksize == Nothing = [basef]
		| otherwise = map (basef ++ ) chunkStream

{- Given a list of destinations to use, chunks the data according to the
 - ChunkSize, and runs the storer action to store each chunk. Returns
 - the destinations where data was stored, or [] on error.
 -
 - This buffers each chunk in memory.
 - More optimal versions of this can be written, that rely
 - on L.toChunks to split the lazy bytestring into chunks (typically
 - smaller than the ChunkSize), and eg, write those chunks to a Handle.
 - But this is the best that can be done with the storer interface that
 - writes a whole L.ByteString at a time.
 -}
storeChunked :: ChunkSize -> [FilePath] -> (FilePath -> L.ByteString -> IO ()) -> L.ByteString -> IO [FilePath]
storeChunked chunksize dests storer content = either onerr return
	=<< (E.try (go chunksize dests) :: IO (Either E.SomeException [FilePath]))
  where
	go _ [] = return [] -- no dests!?
	go Nothing (d:_) = do
		storer d content
		return [d]
	go (Just sz) _
		-- always write a chunk, even if the data is 0 bytes
		| L.null content = go Nothing dests
		| otherwise = storechunks sz [] dests content
		
	onerr e = do
		print e
		return []
	
	storechunks _ _ [] _ = return [] -- ran out of dests
	storechunks sz useddests (d:ds) b
		| L.null b = return $ reverse useddests
		| otherwise = do
			let (chunk, b') = L.splitAt sz b
			storer d chunk
			storechunks sz (d:useddests) ds b'

{- Writes a series of chunks to a file. The feeder is called to get
 - each chunk. -}
meteredWriteFileChunks :: MeterUpdate -> FilePath -> [v] -> (v -> IO L.ByteString) -> IO ()
meteredWriteFileChunks meterupdate dest chunks feeder =
	withBinaryFile dest WriteMode $ \h ->
		forM_ chunks $ \c ->
			meteredWrite meterupdate h =<< feeder c
