{- git-annex deferred parse values
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleInstances #-}

module Types.DeferredParse where

import Annex
import Common

import Options.Applicative.Types

-- Some values cannot be fully parsed without performing an action.
-- The action may be expensive, so it's best to call finishParse on such a
-- value before using getParsed repeatedly.
data DeferredParse a = DeferredParse (Annex a) | ReadyParse a

class DeferredParseClass a where
	finishParse :: a -> Annex a

getParsed :: DeferredParse a -> Annex a
getParsed (DeferredParse a) = a
getParsed (ReadyParse a) = pure a

instance DeferredParseClass (DeferredParse a) where
	finishParse (DeferredParse a) = ReadyParse <$> a
	finishParse (ReadyParse a) = pure (ReadyParse a)

instance DeferredParseClass (Maybe (DeferredParse a)) where
	finishParse Nothing = pure Nothing
	finishParse (Just v) = Just <$> finishParse v

instance DeferredParseClass [DeferredParse a] where
	finishParse v = mapM finishParse v

-- Use when the Annex action modifies Annex state.
type GlobalSetter = DeferredParse ()

globalOpt :: Annex () -> Parser Bool -> Parser GlobalSetter
globalOpt setter parser = go <$> parser
  where
	go False = ReadyParse ()
	go True = DeferredParse setter

globalSetter :: (v -> Annex ()) -> Parser v -> Parser GlobalSetter
globalSetter setter parser = DeferredParse . setter <$> parser
