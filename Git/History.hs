{-# LANGUAGE BangPatterns #-}

{- git commit history interface
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Git.History where

import Common
import Git
import Git.Command
import Git.Sha

import qualified Data.Set as S

data History t = History t (S.Set (History t))
	deriving (Show, Eq, Ord)

mapHistory :: (Ord a, Ord b) => (a -> b) -> History a -> History b
mapHistory f (History t s) = History (f t) (S.map (mapHistory f) s)

historyDepth :: History t -> Integer
historyDepth (History _ s)
	| S.null s = 1
	| otherwise = 1 + maximum (map historyDepth (S.toList s))

truncateHistoryToDepth :: Ord t => Integer -> History t -> History t
truncateHistoryToDepth n (History t ps) = History t (go 1 ps)
  where
	go depth s
		| depth >= n = S.empty
		| otherwise =
			let depth' = succ depth
			in flip S.map s $ \(History t' s') ->
				History t' (go depth' s')

{- Gets a History of trees from commits starting with the provided commit,
 - and down to the requested depth. -}
getTreeHistoryToDepth :: Integer -> Ref -> Repo -> IO (Maybe (History Sha))
getTreeHistoryToDepth n commit r = do
	(_, Just inh, _, pid) <- createProcess (gitCreateProcess params r)
		{ std_out = CreatePipe }
	!h <- fmap (truncateHistoryToDepth n) 
		. build Nothing 
		. map treecommitparents 
		. lines
		<$> hGetContents inh
	hClose inh
	void $ waitForProcess pid
	return h
  where
	build h [] = fmap (mapHistory fst) h
	build _ (Nothing:_) = Nothing
	build Nothing (Just v:rest) =
		build (Just (History v S.empty)) rest
	build (Just h) (Just v:rest) =
		let h' = traverseadd v h
		in build (Just h') $
			-- detect when all parents down to desired depth
			-- have been found, and avoid processing any more,
			-- this makes it much faster when there is a lot of
			-- history.
			if parentsfound 1 h' then [] else rest

	traverseadd v@(t, (c, ps)) (History v'@(t', (c', ps')) s)
		| c `elem` ps' =
			let ps'' = filter (/= c) ps'
			    v'' = (t', (c', ps''))
			in History v'' (S.insert (History v S.empty) s)
		| otherwise = History v' (S.map (traverseadd v) s)

	parentsfound depth (History (_t, (_c, ps)) s)
		| not (null ps) = False
		| null ps && depth == n = True
		| depth >= n = True
		| otherwise = all (parentsfound (succ depth)) (S.toList s)

	params =
		[ Param "log"
		, Param (fromRef commit)
		, Param "--full-history"
		, Param "--no-abbrev"
		, Param "--format=%T %H %P"
		]
	
	treecommitparents l = case map extractSha (splitc ' ' l) of
		(Just t:Just c:ps) -> Just (t, (c, catMaybes ps))
		_ -> Nothing
