{- git-annex numcopies log
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Logs.NumCopies (
	setGlobalNumCopies,
	getGlobalNumCopies,
	globalNumCopiesLoad,
) where

import Common.Annex
import qualified Annex
import Types.NumCopies
import Logs
import Logs.SingleValue

instance SingleValueSerializable NumCopies where
	serialize (NumCopies n) = show n
	deserialize = NumCopies <$$> readish

setGlobalNumCopies :: NumCopies -> Annex ()
setGlobalNumCopies = setLog numcopiesLog

{- Value configured in the numcopies log. Cached for speed. -}
getGlobalNumCopies :: Annex (Maybe NumCopies)
getGlobalNumCopies = maybe globalNumCopiesLoad (return . Just)
	=<< Annex.getState Annex.globalnumcopies

globalNumCopiesLoad :: Annex (Maybe NumCopies)
globalNumCopiesLoad = do
	v <- getLog numcopiesLog
	Annex.changeState $ \s -> s { Annex.globalnumcopies = v }
	return v
