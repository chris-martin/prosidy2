{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Prosidy.Internal.Optics
    ( Optic'
    , Iso'
    , Lens
    , Lens'
    , Prism'
    , Affine'
    , Traversal'
    , iso
    , lens
    , prism
    , affine
    , nullAffine
    , view
    , views
    , preview
    , review
    , over
    , assign
    )
where

import           Data.Profunctor                ( Profunctor(dimap)
                                                , Choice(..)
                                                , Strong(..)
                                                )
import           Data.Functor.Const             ( Const(..) )
import           Data.Monoid                    ( First(..)
                                                , Endo(..)
                                                )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Tagged                    ( Tagged(..) )

type Optic' p f s a = p a (f a) -> p s (f s)

type Iso' s a = forall p f . Profunctor p => Functor f => Optic' p f s a
type Lens s t a b
    = forall p f . Strong p => Functor f => p a (f b) -> p s (f t)
type Lens' s a = forall p f . Strong p => Functor f => Optic' p f s a
type Prism' s a = forall p f . Choice p => Applicative f => Optic' p f s a
type Affine' s a
    = forall p f . Strong p => Choice p => Applicative f => Optic' p f s a
type Traversal' s a = forall f . Applicative f => Optic' (->) f s a

iso :: (s -> a) -> (a -> s) -> Iso' s a
iso get set = dimap get (fmap set)
{-# INLINE iso #-}

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = dimap into outof . second'
  where
    into x = (x, get x)
    outof (x, f) = fmap (set x) f
{-# INLINE lens #-}

prism :: (a -> s) -> (s -> Maybe a) -> Prism' s a
prism set get = dimap lhs rhs . right'
  where
    lhs x = maybe (Left x) Right (get x)
    rhs = either pure (fmap set)
{-# INLINE prism #-}

affine :: (s -> Maybe a) -> (s -> a -> s) -> Affine' s a
affine get set = dimap lhs rhs . right' . second'
  where
    lhs x = maybe (Left x) (Right . (x, )) $ get x
    rhs (Left  x     ) = pure x
    rhs (Right (x, f)) = set x <$> f
{-# INLINE affine #-}

nullAffine :: Affine' s a
nullAffine = affine (const Nothing) const
{-# INLINE nullAffine #-}

view :: Lens' s a -> s -> a
view f = getConst . f Const
{-# INLINE view #-}

views :: Traversal' s a -> s -> [a]
views f = flip appEndo [] . getConst . f (Const . Endo . (:))
{-# INLINE views #-}

preview :: Optic' (->) (Const (First a)) s a -> s -> Maybe a
preview f = getFirst . getConst . f (Const . First . Just)
{-# INLINE preview #-}

over :: Optic' (->) Identity s a -> (a -> a) -> s -> s
over t f = runIdentity . t (Identity . f)
{-# INLINE over #-}

assign :: Optic' (->) Identity s a -> a -> s -> s
assign t = over t . const
{-# INLINE assign #-}

review :: Prism' s a -> a -> s
review p = runIdentity . unTagged . p . Tagged . Identity
{-# INLINE review #-}
