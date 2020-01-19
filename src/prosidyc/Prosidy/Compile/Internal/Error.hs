{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
module Prosidy.Compile.Internal.Error
    ( Result(..)
    , ResultT(..)
    , Errors
    , fromEither
    , resultError
    , result
    , resultM
    , foldResult
    , mapErrors
    , eachError
    , singleton
    , MonadResult(..)
      -- * Re-exports
    , Hashable
    , HashSet
    , Generic
    )
where

import           Data.HashSet                   ( HashSet )
import           GHC.Generics                   ( Generic )
import           Data.Hashable                  ( Hashable )
import           Control.Applicative            ( Alternative(..)
                                                , liftA2
                                                )
import           Control.Monad.Trans            ( MonadTrans(..)
                                                , MonadIO(..)
                                                )
import           Control.Monad.Reader           ( MonadReader(..) )
import           Control.Monad.Writer           ( MonadWriter(..) )
import           Control.Monad.State            ( MonadState(..) )
import           Control.Monad.Fix              ( MonadFix(..) )
import Control.Exception (Exception(..))
import Data.List (intercalate)

import qualified Data.HashSet                  as HashSet

newtype Errors e = Errors (HashSet e)
  deriving stock (Show, Eq, Generic)
  deriving newtype (Foldable, Monoid, Semigroup)

instance Exception e => Exception (Errors e) where
    displayException (Errors es)
        | errorCount == 0 = "Empty error set"
        | errorCount == 1 = foldMap displayException es
        | otherwise       = 
            "Encountered " <> show errorCount <> " errors:\n===\n" <>
            intercalate "\n---\n" (displayException <$> HashSet.toList es)
        where
          errorCount = HashSet.size es

data Result e a =
    Fail (Errors e)
  | Ok a
  deriving (Show, Eq, Generic)

instance (Eq e, Hashable e, Semigroup a) => Semigroup (Result e a) where
    lhs <> rhs = case lhs of
        Fail es -> case rhs of
            Ok   _   -> Fail es
            Fail es' -> Fail (es <> es')
        Ok x -> case rhs of
            Fail es -> Fail es
            Ok   y  -> Ok (x <> y)

instance (Eq e, Hashable e, Monoid a) => Monoid (Result e a) where
    mempty = Ok mempty

instance Foldable (Result e) where
    foldMap = foldResult mempty

instance Functor (Result e) where
    fmap fn (Ok   x ) = Ok (fn x)
    fmap _  (Fail es) = Fail es

instance (Eq e, Hashable e) => Applicative (Result e) where
    pure = Ok

    lhs <*> rhs = case lhs of
        Fail es -> case rhs of
            Fail es' -> Fail (es <> es')
            Ok   _   -> Fail es
        Ok f -> case rhs of
            Fail es -> Fail es
            Ok   x  -> Ok (f x)

instance (Eq e, Hashable e) => Alternative (Result e) where
    empty = Fail (Errors HashSet.empty)

    lhs <|> rhs = case lhs of
        Ok   x  -> Ok x
        Fail es -> case rhs of
            Ok   x   -> Ok x
            Fail es' -> Fail (es <> es')

instance (Eq e, Hashable e) => Traversable (Result e) where
    traverse f (Ok   x) = Ok <$> f x
    traverse _ (Fail f) = pure (Fail f)

newtype ResultT e m a = ResultT
    { runResultT :: m (Result e a) }

instance Functor m => Functor (ResultT e m) where
    fmap fn = ResultT . fmap (fmap fn) . runResultT

instance (Hashable e, Eq e, Applicative m) => Applicative (ResultT e m) where
    pure = ResultT . pure . Ok
    lhs <*> rhs = ResultT $ do
        lhs' <- runResultT lhs
        rhs' <- runResultT rhs
        pure $ lhs' <*> rhs'

instance (Hashable e, Eq e, Applicative m) => Alternative (ResultT e m) where
    empty = ResultT $ pure empty

    ResultT lhs <|> ResultT rhs = ResultT $ liftA2 (<|>) lhs rhs

instance (Hashable e, Eq e, Monad m) => Monad (ResultT e m) where
    ResultT m >>= f = ResultT $ do
        value <- m
        foldResult (pure . Fail) (runResultT . f) value

instance (Eq e, Hashable e, MonadFix m) => MonadFix (ResultT e m) where
    mfix f = ResultT $ mfix (runResultT . f . foldResult (const die) id)
      where
        die = error "mfix (ResultT): inner computation returned Fail value"
    {-# INLINE mfix #-}

instance (Eq e, Hashable e, MonadIO m) => MonadIO (ResultT e m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance (Eq e, Hashable e, MonadReader r m) => MonadReader r (ResultT e m) where
    local f = resultM (local f)
    reader = lift . reader
    {-# INLINE local #-}
    {-# INLINE reader #-}

instance (Eq e, Hashable e, MonadWriter w m) => MonadWriter w (ResultT e m) where
    listen = resultM listen
    pass   = resultM pass
    writer = lift . writer
    {-# INLINE listen #-}
    {-# INLINE pass #-}
    {-# INLINE writer #-}

instance (Eq e, Hashable e, MonadState s m) => MonadState s (ResultT e m) where
    state = lift . state
    {-# INLINE state #-}

instance MonadTrans (ResultT e) where
    lift = ResultT . fmap Ok
    {-# INLINE lift #-}

class Monad m => MonadResult e m | m -> e where
    liftResult :: Result e a -> m a

    raiseError :: (Eq e, Hashable e) => e -> m a
    raiseError = liftResult . resultError
    {-# INLINE raiseError #-}

    raiseErrors :: (Eq e, Hashable e, Foldable f) => f e -> m a
    raiseErrors = liftResult . Fail . Errors . foldMap HashSet.singleton
    {-# INLINE raiseErrors #-}

    raiseErrors' :: Errors e -> m a
    raiseErrors' = liftResult . Fail
    {-# INLINE raiseErrors' #-}

instance (Eq e, Hashable e, Monad m) => MonadResult e (ResultT e m) where
    liftResult = ResultT . pure
    {-# INLINE liftResult #-}

instance {-# OVERLAPPABLE #-}
    (Eq e, Hashable e, Monad (t m), MonadTrans t, MonadResult e m) => MonadResult e (t m) where
    liftResult = lift . liftResult
    {-# INLINE liftResult #-}

result
    :: (Eq e, Hashable e, Monad m)
    => (a -> m b)
    -> ResultT e m a
    -> ResultT e m b
result f r = ResultT $ do
    res <- runResultT r
    case res of
        Fail es -> pure $ Fail es
        Ok   x  -> runResultT . lift $ f x
{-# INLINE result #-}

resultM
    :: (Eq e, Hashable e, Monad m)
    => (m a -> m b)
    -> ResultT e m a
    -> ResultT e m b
resultM f r = ResultT $ do
    res <- runResultT r
    case res of
        Fail es -> pure $ Fail es
        Ok   x  -> runResultT . lift . f $ pure x
{-# INLINE resultM #-}

singleton :: (Hashable e) => e -> Errors e
singleton = Errors . HashSet.singleton

eachError
    :: (Hashable e, Hashable e', Eq e') => (e -> e') -> Errors e -> Errors e'
eachError f (Errors es) = Errors $ HashSet.map f es

mapErrors
    :: (Hashable e, Hashable e', Eq e')
    => (e -> e')
    -> Result e a
    -> Result e' a
mapErrors f (Fail (Errors es)) = Fail . Errors $ HashSet.map f es
mapErrors _ (Ok   ok         ) = Ok ok

foldResult :: (Errors e -> b) -> (a -> b) -> Result e a -> b
foldResult onFail _    (Fail es) = onFail es
foldResult _      onOk (Ok   x ) = onOk x

fromEither :: Hashable e => Either e a -> Result e a
fromEither = either resultError Ok

resultError :: Hashable e => e -> Result e a
resultError = Fail . Errors . HashSet.singleton
