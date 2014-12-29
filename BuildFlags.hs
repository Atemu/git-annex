{- git-annex build flags reporting
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module BuildFlags where

buildFlags :: [String]
buildFlags = filter (not . null)
	[ ""
#ifdef WITH_ASSISTANT
	, "Assistant"
#else
#warning Building without the assistant.
#endif
#ifdef WITH_WEBAPP
	, "Webapp"
#else
#warning Building without the webapp. You probably need to install Yesod..
#endif
#ifdef WITH_WEBAPP_SECURE
	, "Webapp-secure"
#endif
#ifdef WITH_PAIRING
	, "Pairing"
#else
#warning Building without local pairing.
#endif
#ifdef WITH_TESTSUITE
	, "Testsuite"
#else
#warning Building without the testsuite.
#endif
#ifdef WITH_S3
	, "S3"
#else
#warning Building without S3.
#endif
#ifdef WITH_WEBDAV
	, "WebDAV"
#else
#warning Building without WebDAV.
#endif
#ifdef WITH_INOTIFY
	, "Inotify"
#endif
#ifdef WITH_FSEVENTS
	, "FsEvents"
#endif
#ifdef WITH_KQUEUE
	, "Kqueue"
#endif
#ifdef WITH_DBUS
	, "DBus"
#endif
#ifdef WITH_DESKTOP_NOTIFY
	, "DesktopNotify"
#endif
#ifdef WITH_XMPP
	, "XMPP"
#else
#warning Building without XMPP.
#endif
#ifdef WITH_DNS
	, "DNS"
#endif
#ifdef WITH_FEED
	, "Feeds"
#else
#warning Building without Feeds.
#endif
#ifdef WITH_QUVI
	, "Quvi"
#else
#warning Building without quvi.
#endif
#ifdef WITH_TDFA
	, "TDFA"
#endif
#ifdef WITH_CRYPTOHASH
	, "CryptoHash"
#else
#warning Building without CryptoHash.
#endif
#ifdef WITH_TORRENTPARSER
	, "TorrentParser"
#else
#warning Building without haskell torrent library; will instead use btshowmetainfo to parse torrent files.
#endif
#ifdef WITH_EKG
	, "EKG"
#endif
	]
