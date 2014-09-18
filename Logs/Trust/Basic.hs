{- git-annex trust log, basics
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Trust.Basic (
	module X,
	trustSet,
) where

import Data.Time.Clock.POSIX

import Common.Annex
import Types.TrustLevel
import qualified Annex.Branch
import qualified Annex
import Logs
import Logs.UUIDBased
import Logs.Trust.Pure as X

{- Changes the trust level for a uuid in the trustLog. -}
trustSet :: UUID -> TrustLevel -> Annex ()
trustSet uuid@(UUID _) level = do
	ts <- liftIO getPOSIXTime
	Annex.Branch.change trustLog $
		showLog showTrustLog .
			changeLog ts uuid level .
				parseLog (Just . parseTrustLog)
	Annex.changeState $ \s -> s { Annex.trustmap = Nothing }
trustSet NoUUID _ = error "unknown UUID; cannot modify"
