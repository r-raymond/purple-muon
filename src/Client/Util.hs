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

module Client.Util
    ( liftMaybe
    , liftEitherWith
    , liftEither
    ) where

import Protolude

liftMaybe :: (MonadError e m) => e -> Maybe a -> m a
liftMaybe err Nothing = throwError err
liftMaybe _ (Just s) = return s

liftEitherWith :: (MonadError e m) => (f -> e) -> Either f a -> m a
liftEitherWith g (Left e) = throwError $ g e
liftEitherWith _ (Right s) = return s

liftEither :: (MonadError e m) => Either e a -> m a
liftEither = liftEitherWith identity

