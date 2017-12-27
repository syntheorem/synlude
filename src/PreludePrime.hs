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
, Monoid(mempty, mappend)
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
, error
, undefined
, seq
, ($!)
, any
, all
, Show(show)
, Read
, IO

-- Other reexports
, Control.DeepSeq.NFData
, Control.DeepSeq.deepseq
, Control.DeepSeq.force
, (Control.DeepSeq.$!!)

, Control.Exception.Exception(toException, fromException, displayException)
, Control.Exception.SomeException(SomeException)
, Control.Exception.throw
, Control.Exception.throwIO
, Control.Exception.try
, Control.Exception.catch
, Control.Exception.evaluate

, (Control.Monad.>=>)
, (Control.Monad.<=<)
, Control.Monad.forever
, Control.Monad.void
, Control.Monad.join
, Control.Monad.foldM
, Control.Monad.foldM_
, Control.Monad.guard
, Control.Monad.when
, Control.Monad.unless
, Control.Monad.Fail.MonadFail(fail)

, Data.Foldable.for_
, Data.Foldable.traverse_
, Data.Foldable.Foldable
    ( fold, foldMap
    , foldr, foldl
    , foldr', foldl'
    , toList, elem
    , null, length
    )

, Data.Bool.bool
, (Data.Function.&)
, (Data.Ratio.%)
, Data.Traversable.for
, Data.Proxy.Proxy(Proxy)

-- Redefined functions
, (<$!>)
, (<$!!>)
, sequence_
, sequence
, replicateA
, replicateA_
, map
, (++)
, filter
, filterMap
, filterA
, filterMapA
, concat
, concatMap
, tryRead
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

-- | Strict version of '<$>'.
(<$!>) :: Functor f => (a -> b) -> f a -> f b
(<$!>) f = fmap (\a -> let b = f a in b `seq` b)
infixl 4 <$!>
{-# INLINE (<$!>) #-}

-- | Deeply strict version of '<$>'.
(<$!!>) :: (Functor f, NFData a) => (a -> b) -> f a -> f b
(<$!!>) f = fmap (\a -> let b = f a in b `deepseq` b)
infixl 4 <$!!>
{-# INLINE (<$!!>) #-}

-- | An alias for 'Data.Foldable.sequenceA_'.
sequence_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequence_ = Data.Foldable.sequenceA_
{-# INLINE sequence_ #-}

-- | An alias for 'Data.Traversable.sequenceA'.
sequence :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequence = Data.Traversable.sequenceA
{-# INLINE sequence #-}

-- | An alias for 'Control.Monad.replicateM'.
replicateA :: Applicative f => Int -> f a -> f [a]
replicateA = Control.Monad.replicateM
{-# INLINE replicateA #-}

-- | An alias for 'Control.Monad.replicateM_'.
replicateA_ :: Applicative f => Int -> f a -> f ()
replicateA_ = Control.Monad.replicateM_
{-# INLINE replicateA_ #-}

-- | An alias for 'fmap'.
map :: Functor f => (a -> b) -> f a -> f b
map = Prelude.fmap
{-# INLINE map #-}

-- | An infix alias for 'mappend'.
(++) :: Monoid a => a -> a -> a
(++) = Prelude.mappend
infixr 5 ++
{-# INLINE (++) #-}

-- | Construct a list of only the elements satisfying the provided predicate.
filter :: Foldable t => (a -> Bool) -> t a -> [a]
filter f = foldr (\a -> bool id (a:) (f a)) []
{-# RULES filter = Prelude.filter #-}

-- | Map over a list and filter out the 'Nothing' values.
filterMap :: Foldable t => (a -> Maybe b) -> t a -> [b]
filterMap f = foldr (\a -> maybe id (:) (f a)) []
{-# RULES filterMap = Data.Maybe.mapMaybe #-}

-- | A version of 'filter' in an 'Applicative' context.
filterA :: (Applicative f, Foldable t) => (a -> f Bool) -> t a -> f [a]
filterA f = foldr (\a as -> bool id (a:) <$> f a <*> as) []
{-# RULES filterA = Control.Monad.filterM #-}

-- | A version of 'filterMap' in an 'Applicative' context.
filterMapA :: (Applicative f, Foldable t) => (a -> f (Maybe b)) -> t a -> f [b]
filterMapA f = foldr (\a bs -> maybe id (:) <$> f a <*> bs) []

-- | 'mappend' all elements of a container.
concat :: (Foldable t, Monoid a) => t a -> a
concat = foldr mappend mempty
{-# RULES concat = Prelude.concat #-}

-- | Map over a container and `mappend` the results.
concatMap :: (Foldable t, Monoid b) => (a -> b) -> t a -> b
concatMap f = foldr (mappend . f) mempty
{-# RULES concatMap = Prelude.concatMap #-}

-- | Read a value, returning 'Nothing' on failure.
tryRead :: Read a => String -> Maybe a
tryRead s = case [ a | (a, rest) <- reads s, null rest ] of
    [a] -> Just a
    _   -> Nothing
