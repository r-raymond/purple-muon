--  Copyright 2016, 2017 Robin Raymond
--
--  This file is part of Purple Muon
--
--  Purple Muon is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  Purple Muon is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Purple Muon.  If not, see <http://www.gnu.org/licenses/>.

{-|
Module      : PurpleMuon.Util.MonadError
Description : MonadError utitilies
Copyright   : (c) Robin Raymond, 2016-2017
License     : GPL-3
Maintainer  : robin@robinraymond.de
Portability : POSIX

This module defines common lifting operations into MonadError.
-}
module PurpleMuon.Util.MonadError
    ( liftMaybe
    , liftEitherWith
    , liftEither
    , mapLeft
    ) where

import Protolude

-- | Map the error type (`Left`) of an `Either`
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

-- | Lift a Maybe into an error monad
liftMaybe :: (MonadError e m) => e          -- ^ Error to be thrown if Nothing
                              -> Maybe a    -- ^ Value that has to be Just
                              -> m a
liftMaybe err Nothing = throwError err
liftMaybe _ (Just s) = return s

-- | Lift Either into error monad
-- This version additionally allows to specify a transformation for the `Left`
-- value.
liftEitherWith :: (MonadError e m)
               => (f -> e)      -- ^ Transformation for left value
               -> Either f a    -- ^ Value that has to be Right
               -> m a
liftEitherWith g (Left e) = throwError $ g e
liftEitherWith _ (Right s) = return s

-- | Lift Either into error monad
-- Throws error on `Left` Value.
liftEither :: (MonadError e m) => Either e a -> m a
liftEither = liftEitherWith identity

