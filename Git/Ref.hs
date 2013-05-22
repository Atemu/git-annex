{- git ref stuff
 -
 - Copyright 2011-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Ref where

import Common
import Git
import Git.Command

import Data.Char (chr)

headRef :: Ref
headRef = Ref "HEAD"

{- Converts a fully qualified git ref into a user-visible string. -}
describe :: Ref -> String
describe = show . base

{- Often git refs are fully qualified (eg: refs/heads/master).
 - Converts such a fully qualified ref into a base ref (eg: master). -}
base :: Ref -> Ref
base = Ref . remove "refs/heads/" . remove "refs/remotes/" . show
  where
	remove prefix s
		| prefix `isPrefixOf` s = drop (length prefix) s
		| otherwise = s

{- Given a directory such as "refs/remotes/origin", and a ref such as
 - refs/heads/master, yields a version of that ref under the directory,
 - such as refs/remotes/origin/master. -}
under :: String -> Ref -> Ref
under dir r = Ref $ dir </> show (base r)

{- Checks if a ref exists. -}
exists :: Ref -> Repo -> IO Bool
exists ref = runBool
	[Param "show-ref", Param "--verify", Param "-q", Param $ show ref]

{- Checks if HEAD exists. It generally will, except for in a repository
 - that was just created. -}
headExists :: Repo -> IO Bool
headExists repo = do
	ls <- lines <$> pipeReadStrict [Param "show-ref", Param "--head"] repo
	return $ any (" HEAD" `isSuffixOf`) ls

{- Get the sha of a fully qualified git ref, if it exists. -}
sha :: Branch -> Repo -> IO (Maybe Sha)
sha branch repo = process <$> showref repo
  where
	showref = pipeReadStrict [Param "show-ref",
		Param "--hash", -- get the hash
		Param $ show branch]
	process [] = Nothing
	process s = Just $ Ref $ firstLine s

{- List of (shas, branches) matching a given ref or refs. -}
matching :: [Ref] -> Repo -> IO [(Sha, Branch)]
matching refs repo =  matching' (map show refs) repo

{- Includes HEAD in the output, if asked for it. -}
matchingWithHEAD :: [Ref] -> Repo -> IO [(Sha, Branch)]
matchingWithHEAD refs repo = matching' ("--head" : map show refs) repo

{- List of (shas, branches) matching a given ref or refs. -}
matching' :: [String] -> Repo -> IO [(Sha, Branch)]
matching' ps repo = map gen . lines <$> 
	pipeReadStrict (Param "show-ref" : map Param ps) repo
  where
	gen l = let (r, b) = separate (== ' ') l
		in (Ref r, Ref b)

{- List of (shas, branches) matching a given ref spec.
 - Duplicate shas are filtered out. -}
matchingUniq :: [Ref] -> Repo -> IO [(Sha, Branch)]
matchingUniq refs repo = nubBy uniqref <$> matching refs repo
  where
	uniqref (a, _) (b, _) = a == b

{- Checks if a String is a legal git ref name.
 -
 - The rules for this are complex; see git-check-ref-format(1) -}
legal :: Bool -> String -> Bool
legal allowonelevel s = all (== False) illegal
  where
	illegal =
		[ any ("." `isPrefixOf`) pathbits
		, any (".lock" `isSuffixOf`) pathbits
		, not allowonelevel && length pathbits < 2
		, contains ".."
		, any (\c -> contains [c]) illegalchars
		, begins "/"
		, ends "/"
		, contains "//"
		, ends "."
		, contains "@{"
		, null s
		]
	contains v = v `isInfixOf` s
	ends v = v `isSuffixOf` s
	begins v = v `isPrefixOf` s

	pathbits = split "/" s
	illegalchars = " ~^:?*[\\" ++ controlchars
	controlchars = chr 0o177 : [chr 0 .. chr (0o40-1)]
