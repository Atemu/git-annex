{- git-annex numcopies type
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.NumCopies where

newtype NumCopies = NumCopies Int
	deriving (Ord, Eq)

fromNumCopies :: NumCopies -> Int
fromNumCopies (NumCopies n) = n
