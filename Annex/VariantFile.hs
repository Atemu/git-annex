{- git-annex .variant files for automatic merge conflict resolution
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.VariantFile where

import Annex.Common
import Utility.Hash

import qualified Data.ByteString.Lazy as L

variantMarker :: String
variantMarker = ".variant-"

mkVariant :: FilePath -> String -> FilePath
mkVariant file variant = takeDirectory file
	</> dropExtension (takeFileName file)
	++ variantMarker ++ variant
	++ takeExtension file

{- The filename to use when resolving a conflicted merge of a file,
 - that points to a key.
 -
 - Something derived from the key needs to be included in the filename,
 - but rather than exposing the whole key to the user, a very weak hash
 - is used. There is a very real, although still unlikely, chance of
 - conflicts using this hash.
 -
 - In the event that there is a conflict with the filename generated
 - for some other key, that conflict will itself be handled by the
 - conflicted merge resolution code. That case is detected, and the full
 - key is used in the filename.
 -}
variantFile :: FilePath -> Key -> FilePath
variantFile file key
	| doubleconflict = mkVariant file (keyFile key)
	| otherwise = mkVariant file (shortHash $ serializeKey' key)
  where
	doubleconflict = variantMarker `isInfixOf` file

shortHash :: L.ByteString -> String
shortHash = take 4 . show . md5
