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

