{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Prosidy.Compile.Internal.Util where

import           Prosidy.Compile.Internal.Error ( MonadResult(..)
                                                , raiseErrors
                                                )

import qualified Control.Lens                  as L
import           Control.Lens.Operators
import           Control.Monad.State            ( MonadState )
import           Data.Foldable                  ( for_ )
import           Control.Monad                  ( unless )
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )
import           Data.Hashable                  ( Hashable )
import           Type.Reflection                ( Typeable
                                                , SomeTypeRep(..)
                                                , typeRep
                                                )
import           Control.Monad.Cont             ( ContT
                                                , runContT
                                                , callCC
                                                )

withCC :: Monad m => ((r -> ContT r m r) -> ContT r m r) -> m r
withCC = \f -> runContT (callCC f) pure

setOnce
    :: (MonadState state m, MonadResult error m, Eq a, Eq error, Hashable error)
    => L.LensLike' ((,) (Maybe a)) state (Maybe a)
    -> (a -> error)
    -> a
    -> m ()
setOnce s failBy new = do
    maybeOld <- s <<.= Just new
    for_ maybeOld
        $ \old -> unless (new == old) $ raiseErrors [failBy new, failBy old]

setOnceFail
    :: (MonadState state m, MonadError error m, Eq a)
    => L.LensLike' ((,) (Maybe a)) state (Maybe a)
    -> error
    -> a
    -> m ()
setOnceFail s e new = do
    maybeOld <- s <<.= Just new
    for_ maybeOld $ \old -> unless (new == old) $ throwError e

typeOf :: forall a . Typeable a => SomeTypeRep
typeOf = SomeTypeRep (typeRep @a)

