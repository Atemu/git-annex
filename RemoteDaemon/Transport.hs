{- git-remote-daemon transports
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module RemoteDaemon.Transport where

import RemoteDaemon.Types
import qualified RemoteDaemon.Transport.Ssh
import qualified Git.GCrypt

import qualified Data.Map as M

-- Corresponds to uriScheme
type TransportScheme = String

remoteTransports :: M.Map TransportScheme Transport
remoteTransports = M.fromList
	[ ("ssh:", RemoteDaemon.Transport.Ssh.transport)
	, (Git.GCrypt.urlScheme, RemoteDaemon.Transport.Ssh.transport)
	]
