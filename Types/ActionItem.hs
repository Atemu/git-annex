{- items that a command can act on
 -
 - Copyright 2016-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Types.ActionItem where

import Key
import Types.Transfer
import Git.FilePath

import Data.Maybe

data ActionItem 
	= ActionItemAssociatedFile AssociatedFile Key
	| ActionItemKey Key
	| ActionItemBranchFilePath BranchFilePath Key
	| ActionItemFailedTransfer Transfer TransferInfo
	| ActionItemWorkTreeFile FilePath
	| ActionItemOther (Maybe String)

class MkActionItem t where
	mkActionItem :: t -> ActionItem

instance MkActionItem ActionItem where
	mkActionItem = id

instance MkActionItem (AssociatedFile, Key) where
	mkActionItem = uncurry ActionItemAssociatedFile

instance MkActionItem (Key, AssociatedFile) where
	mkActionItem = uncurry $ flip ActionItemAssociatedFile

instance MkActionItem (Key, FilePath) where
	mkActionItem (key, file) = ActionItemAssociatedFile (AssociatedFile (Just file)) key

instance MkActionItem (FilePath, Key) where
	mkActionItem (file, key) = mkActionItem (key, file)

instance MkActionItem Key where
	mkActionItem = ActionItemKey

instance MkActionItem (BranchFilePath, Key) where
	mkActionItem = uncurry ActionItemBranchFilePath

instance MkActionItem (Transfer, TransferInfo) where
	mkActionItem = uncurry ActionItemFailedTransfer

actionItemDesc :: ActionItem -> String
actionItemDesc (ActionItemAssociatedFile (AssociatedFile (Just f)) _) = f
actionItemDesc (ActionItemAssociatedFile (AssociatedFile Nothing) k) = 
	serializeKey k
actionItemDesc (ActionItemKey k) = serializeKey k
actionItemDesc (ActionItemBranchFilePath bfp _) = descBranchFilePath bfp
actionItemDesc (ActionItemFailedTransfer t i) = actionItemDesc $
	ActionItemAssociatedFile (associatedFile i) (transferKey t)
actionItemDesc (ActionItemWorkTreeFile f) = f
actionItemDesc (ActionItemOther s) = fromMaybe "" s

actionItemKey :: ActionItem -> Maybe Key
actionItemKey (ActionItemAssociatedFile _ k) = Just k
actionItemKey (ActionItemKey k) = Just k
actionItemKey (ActionItemBranchFilePath _ k) = Just k
actionItemKey (ActionItemFailedTransfer t _) = Just (transferKey t)
actionItemKey (ActionItemWorkTreeFile _) = Nothing
actionItemKey (ActionItemOther _) = Nothing

actionItemWorkTreeFile :: ActionItem -> Maybe FilePath
actionItemWorkTreeFile (ActionItemAssociatedFile (AssociatedFile af) _) = af
actionItemWorkTreeFile (ActionItemWorkTreeFile f) = Just f
actionItemWorkTreeFile _ = Nothing

actionItemTransferDirection :: ActionItem -> Maybe Direction
actionItemTransferDirection (ActionItemFailedTransfer t _) = Just $
	transferDirection t
actionItemTransferDirection _ = Nothing
