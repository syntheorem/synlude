module PreludePrime
(
-- Prelude reexports
  Bool
, (&&)
, (||)
, not
, otherwise
, Maybe(Just, Nothing)
, maybe
, Either(Left, Right)
, either
, Ordering(LT, EQ, GT)
, Char
, String
, Eq((==), (/=))
, Ord(compare, (<), (<=), (>), (>=))
, Int
, Integer
, Float
, Double
, Rational
, Word
, Num((+), (-), (*), negate, fromInteger)
, Real(toRational)
, Integral(div, mod, toInteger)
, Fractional((/), fromRational)
, Floating((**))
, subtract
, (^)
, (^^)
, fromIntegral
, realToFrac
, Monoid(mempty, mappend, mconcat)
, Functor(fmap, (<$))
, (<$>)
, Applicative(pure, (<*>), (*>), (<*))
, Monad((>>=), (>>))
, (=<<)
, Traversable(traverse)
, id
, const
, (.)
, flip
, ($)
, until
, error
, undefined
, seq
, ($!)
, reverse

-- Other reexports
, (>=>)
, (<=<)
, forever
, void
, join
, Control.Monad.guard
, Control.Monad.when
, Control.Monad.unless
, Control.Monad.Fail.MonadFail(fail)
, Data.Bool.bool
, Data.Foldable.Foldable
    ( fold, foldMap
    , foldr, foldl
    , foldr', foldl'
    , toList, elem
    , null, length
    )
, Data.Foldable.for_
, Data.Foldable.traverse_
, (Data.Ratio.%)
, Data.Traversable.for
, Data.Proxy.Proxy(Proxy)

-- Redefined functions
, sequence_
, sequence
, map
, (++)
, filter
, filterMap
, filterA
, filterMapA
) where

import qualified Prelude
import qualified Control.Applicative
import qualified Control.Monad
import qualified Control.Monad.Fail
import qualified Data.Bool
import qualified Data.Foldable
import qualified Data.Maybe
import qualified Data.Ratio
import qualified Data.Traversable
import qualified Data.Proxy

import Control.Applicative (Alternative(empty, (<|>)))
import Control.Monad (guard)
import Data.Bool (bool)
import Prelude (Monoid, Foldable(foldr), Traversable, Applicative(pure, (<*>)), Maybe(Just, Nothing), (<$>), maybe)

{-# INLINE sequence_ #-}
sequence_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequence_ = Data.Foldable.sequenceA_

{-# INLINE sequence #-}
sequence :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequence = Data.Traversable.sequenceA

{-# INLINE map #-}
map :: Functor f => (a -> b) -> f a -> f b
map = Prelude.fmap

{-# INLINE (++) #-}
(++) :: Monoid a => a -> a -> a
(++) = Prelude.mappend

{-# RULES "filter/List" filter = filterList #-}
filter :: (Foldable t, Alternative m) => (a -> Bool) -> t a -> m a
filter f = foldr (\a -> bool id (pure a <|>) (f a)) empty

filterList :: Foldable t => (a -> Bool) -> t a -> [a]
filterList f = foldr (\a -> bool id (a:) (f a)) []

{-# RULES "filterMap/List" filterMap = filterMapList #-}
filterMap :: (Foldable t, Alternative m) => (a -> Maybe b) -> t a -> m b
filterMap f = foldr (\a -> maybe id ((<|>) . pure) (f a)) empty

filterMapList :: Foldable t => (a -> Maybe b) -> t a -> [b]
filterMapList f = foldr (\a -> maybe id (:) (f a)) []

{-# RULES "filterA/List" filterA = filterAList #-}
filterA :: (Applicative f, Foldable t, Alternative m) => (a -> f Bool) -> t a -> f (m a)
filterA f = foldr (\a as -> bool id (pure a <|>) <$> f a <*> as) empty

filterAList :: (Applicative f, Foldable t) => (a -> f Bool) -> t a -> f [a]
filterAList f = foldr (\a as -> bool id (a:) <$> f a <*> as) []

{-# RULES "filterA/List" filterA = filterMapAList #-}
filterMapA :: (Applicative f, Foldable t, Alternative m) => (a -> f (Maybe b)) -> t a -> f (m b)
filterMapA f = foldr (\a bs -> maybe id ((<|>) . pure) <$> f a <*> bs) empty

filterMapAList :: (Applicative f, Foldable t) => (a -> f (Maybe b)) -> t a -> f [b]
filterMapAList f = foldr (\a bs -> maybe id (:) <$> f a <*> bs) []
