{- git-annex assistant dropping of unwanted content
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Drop (
	handleDrops,
	handleDropsFrom,
) where

import Assistant.Common
import Assistant.DaemonStatus
import Annex.Drop (handleDropsFrom, Reason)
import Logs.Location
import CmdLine.Action

{- Drop from local and/or remote when allowed by the preferred content and
 - numcopies settings. -}
handleDrops :: Reason -> Bool -> Key -> AssociatedFile -> Maybe Remote -> Assistant ()
handleDrops reason fromhere key f knownpresentremote = do
	syncrs <- syncDataRemotes <$> getDaemonStatus
	locs <- liftAnnex $ loggedLocations key
	liftAnnex $ handleDropsFrom locs syncrs reason fromhere key f knownpresentremote callCommandAction
