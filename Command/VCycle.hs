{- git-annex command
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.VCycle where

import Common.Annex
import Command
import Annex.View
import Types.View
import Logs.View
import Command.View (checkoutViewBranch)

cmd :: [Command]
cmd = [notBareRepo $ notDirect $
	command "vcycle" paramNothing seek SectionUtility
	"switch view to next layout"]

seek :: CommandSeek
seek = withNothing start

start ::CommandStart
start = go =<< currentView
  where
	go Nothing = error "Not in a view."
	go (Just v) = do
		showStart "vcycle" ""
		let v' = v { viewComponents = vcycle [] (viewComponents v) }
		if v == v'
			then do
				showNote "unchanged"
				next $ next $ return True
			else next $ next $ checkoutViewBranch v' narrowView

	vcycle rest (c:cs)
		| viewVisible c = rest ++ cs ++ [c]
		| otherwise = vcycle (c:rest) cs
	vcycle rest c = rest ++ c
