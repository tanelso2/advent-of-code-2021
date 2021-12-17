{-# LANGUAGE RankNTypes #-}
module Lens
  (
  Lens
  , Lens'
  , Traversal
  , Traversal'
  , Setter
  , Setter'
  , view
  , over
  , set
  , sets
  ) where

import Control.Applicative
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Contravariant
import Data.Functor

type Lens s t a b = forall f. (Functor f) => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

type Traversal s t a b = forall f. (Applicative f) => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

type Setter s t a b = (a -> Identity b) -> s -> Identity t
type Setter' s a = Setter s s a a

type Getter s a = forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s

over :: Setter s t a b -> (a -> b) -> s -> t
over l f x = runIdentity $ l (Identity . f) x

set :: Setter s t a b -> b -> s -> t
set l y = over l $ const y

sets :: ((a -> b) -> s -> t) -> Setter s t a b
sets f = (\x -> Identity . f (runIdentity . x))

view :: Getter s a -> s -> a
view l x = getConst $ l Const x

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set f s = set s <$> f (get s)

_1 :: Lens (a, x) (b, x) a b
_1 f (a,x) = (\b -> (b,x)) <$> f a

_2 :: Lens (x, a) (x, b) a b
_2 f (x,a) = (\b -> (x,b)) <$> f a
