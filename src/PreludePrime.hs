-- | An alternative prelude for modern Haskell.
module PreludePrime
(
-- * Function
  Data.Function.id
, Data.Function.const
, Data.Function.flip
, Data.Function.fix
, Prelude.until
, (Data.Function..)
, (Data.Function.$)
, (Data.Function.&)

-- * Bool
, Data.Bool.Bool(True, False)
, Data.Bool.bool
, Data.Bool.not
, (Data.Bool.&&)
, (Data.Bool.||)
, (<&&>)
, (<||>)
, Data.Bool.otherwise

-- * Comparison
, Data.Eq.Eq((==), (/=))
, Data.Ord.Ordering(LT, EQ, GT)
, Data.Ord.Ord(compare, (<), (<=), (>), (>=), min, max)
, Data.Ord.comparing

-- * Maybe
, Data.Maybe.Maybe(Just, Nothing)
, Data.Maybe.maybe
, Data.Maybe.fromMaybe
, Data.Maybe.isJust
, Data.Maybe.isNothing

-- * Either
, Data.Either.Either(Left, Right)
, Data.Either.either
, Data.Either.isLeft
, Data.Either.isRight

-- * Char
, Data.Char.Char
, Data.String.String
, Data.String.IsString(fromString)

-- * Numeric

-- ** Core types
, Prelude.Int
, Prelude.Word
, Prelude.Float
, Prelude.Double

-- ** Arbitrary precision
, Prelude.Integer
, Prelude.Rational
, (Data.Ratio.%)

-- ** Explicit precision
, Data.Int.Int8
, Data.Int.Int16
, Data.Int.Int32
, Data.Int.Int64
, Data.Word.Word8
, Data.Word.Word16
, Data.Word.Word32
, Data.Word.Word64

-- ** Classes and functions
, Prelude.Num((+), (-), (*), negate, fromInteger)
, Prelude.subtract
, Prelude.Real(toRational)
, Prelude.Integral(quot, rem, div, mod, quotRem, divMod, toInteger)
, Prelude.fromIntegral
, (Prelude.^)
, Prelude.Fractional((/), fromRational)
, Prelude.realToFrac
, (Prelude.^^)
, Prelude.RealFrac(properFraction, truncate, round, ceiling, floor)

-- * Enumerations
, Prelude.Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen, enumFromTo, enumFromThenTo)
, Prelude.Bounded(minBound, maxBound)

-- * Semigroup and Monoid
, Data.Semigroup.Semigroup((<>))
, Data.Monoid.Monoid(mappend, mempty)
, (++)
, concat
, concatMap
, concatMapA

-- * Functor
, Data.Functor.Functor(fmap)
, map
, (Data.Functor.<$>)
, (Data.Functor.<$)
, (Data.Functor.$>)
, Data.Functor.void
, Data.Functor.Const.Const(Const, getConst)
, Data.Functor.Identity.Identity(Identity, runIdentity)

-- * Applicative
, Control.Applicative.Applicative(pure, (<*>))
, (Control.Applicative.*>)
, (Control.Applicative.<*)
, Control.Applicative.liftA2
, Control.Applicative.liftA3
, Control.Monad.forever
, Control.Monad.when
, Control.Monad.unless
, replicateA
, replicateA_

-- * Monad
, Control.Monad.Monad((>>=), (>>), return)
, Control.Monad.Fail.MonadFail(fail)
, (Control.Monad.=<<)
, (Control.Monad.>=>)
, (Control.Monad.<=<)
, Control.Monad.join
, Control.Monad.foldM
, Control.Monad.foldM_

-- * Foldable
, Data.Foldable.Foldable
    ( fold, foldMap
    , foldr, foldl
    , foldr', foldl'
    , toList, elem
    , null, length
    )
, Data.Foldable.for_
, Data.Foldable.traverse_
, sequence_

-- * Traversable
, Data.Traversable.Traversable(traverse)
, Data.Traversable.for
, sequence

-- * Special folds
, Data.Foldable.any
, Data.Foldable.all
, filter
, filterMap
, filterA
, filterMapA

-- * Forcing evaluation
, evaluate
, Prelude.seq
, (Prelude.$!)
, (<$!>)
, Control.DeepSeq.NFData
, Control.DeepSeq.deepseq
, Control.DeepSeq.force
, (Control.DeepSeq.$!!)
, (<$!!>)

-- * Exceptions
, PreludePrime.Exception.undefined
, PreludePrime.Exception.error
, PreludePrime.Exception.errorM
, PreludePrime.Exception.throw
, PreludePrime.Exception.throwM
, PreludePrime.Exception.Exception(toException, fromException, displayException)
, PreludePrime.Exception.SomeException(SomeException)
, GHC.Stack.HasCallStack

-- ** Assertions
-- | See "PreludePrime.Exception" for additional documentation.
, PreludePrime.Exception.assert
, PreludePrime.Exception.assertMsg
, PreludePrime.Exception.assertM
, PreludePrime.Exception.assertMsgM
, PreludePrime.Exception.ensure
, PreludePrime.Exception.ensureMsg
, PreludePrime.Exception.ensureM
, PreludePrime.Exception.ensureMsgM

-- * Showing and reading values
, Text.Show.Show(show)
, Text.Read.Read
, tryRead

-- * IO
-- | Functions which do actual IO should be imported from "System.IO".
, System.IO.IO
, Control.Monad.IO.Class.MonadIO(liftIO)

-- * Type casts and coercions
, Data.Coerce.Coercible
, Data.Coerce.coerce
, Data.Typeable.Typeable
, Data.Typeable.cast

-- * Additional utilities
, Data.Void.Void
, Data.Proxy.Proxy(Proxy)
, GHC.Exts.IsList(fromList)
, GHC.TypeLits.Nat
, GHC.TypeLits.Symbol
, GHC.Generics.Generic

-- * Tracing
-- | Provided in the prelude to avoid having to add and remove @import "Debug.Trace"@ statements when debugging.
-- Also note that I modified some of the combinator functions to be more generally useful.
, Debug.Trace.trace
, traceShow
, Debug.Trace.traceStack
, traceShowStack
, traceIO
, traceStackIO
) where

import qualified Control.Applicative
import qualified Control.DeepSeq
import qualified Control.Exception
import qualified Control.Monad
import qualified Control.Monad.Fail
import qualified Data.Bool
import qualified Data.Char
import qualified Data.Coerce
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Foldable
import qualified Data.Function
import qualified Data.Functor
import qualified Data.Functor.Const
import qualified Data.Functor.Identity
import qualified Data.Int
import qualified Data.Maybe
import qualified Data.Monoid
import qualified Data.Ord
import qualified Data.Ratio
import qualified Data.Semigroup
import qualified Data.String
import qualified Data.Traversable
import qualified Data.Typeable
import qualified Data.Proxy
import qualified Data.Word
import qualified Data.Void
import qualified Debug.Trace
import qualified GHC.Exts
import qualified GHC.Generics
import qualified GHC.Stack
import qualified GHC.TypeLits
import qualified Prelude
import qualified PreludePrime.Exception
import qualified System.IO
import qualified Text.Read
import qualified Text.Show

import Control.DeepSeq (NFData, deepseq)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bool (bool)
import GHC.Exts (build)

import Prelude ( Bool(True, False), Int, String, Maybe(Just, Nothing)
               , maybe, Functor(fmap), (<$>), Applicative(pure, (<*>))
               , Monoid(mappend, mempty), Foldable(foldr, null)
               , Traversable, Read, reads, Show(show), (.), seq
               , id, not, (&&), (||), ($)
               )

-- NOTE: The rules pragmas for the following functions are mainly to
-- make use of existing rules for the list-specific versions in base.
-- There are also "build" rules to enable list fusion optimizations.

-- | A lifted version of '&&'.
--
-- Useful when combined with the 'Applicative' instance for functions,
-- e.g. @(isSpace \<&&\> isControl) c@.
(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = Control.Applicative.liftA2 (&&)
infixr 3 <&&>
{-# INLINE (<&&>) #-}

-- | A lifted version of '||'.
--
-- Useful when combined with the 'Applicative' instance for functions,
-- e.g. @(isSpace \<||\> isControl) c@.
(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = Control.Applicative.liftA2 (||)
infixr 2 <||>
{-# INLINE (<||>) #-}

-- | An infix alias for 'mappend'.
(++) :: Monoid a => a -> a -> a
(++) = Prelude.mappend
infixr 5 ++
{-# INLINE (++) #-}

-- | 'mappend' all elements of a container.
concat :: (Foldable t, Monoid a) => t a -> a
concat = foldr mappend mempty
{-# INLINE[1] concat #-}
{-# RULES "concat/List" concat = Prelude.concat #-}

-- | Map over a container and 'mappend' the results.
concatMap :: (Foldable t, Monoid b) => (a -> b) -> t a -> b
concatMap f = foldr (mappend . f) mempty
{-# INLINE[1] concatMap #-}
{-# RULES "concatMap/List" concatMap = Prelude.concatMap #-}

-- | Map over a container in an 'Applicative' context and 'mappend' the results.
concatMapA :: (Applicative f, Foldable t, Monoid b) => (a -> f b) -> t a -> f b
concatMapA f = foldr (\a b -> mappend <$> f a <*> b) (pure mempty)
{-# INLINE concatMapA #-}

-- | An alias for 'fmap'.
map :: Functor f => (a -> b) -> f a -> f b
map = Prelude.fmap
{-# INLINE map #-}

-- | @'replicateA' n act@ performs the action @n@ times, gathering the results.
replicateA :: Applicative f => Int -> f a -> f [a]
replicateA = Control.Monad.replicateM
{-# INLINE replicateA #-}

-- | Like 'replicateA', but discards the result.
replicateA_ :: Applicative f => Int -> f a -> f ()
replicateA_ = Control.Monad.replicateM_
{-# INLINE replicateA_ #-}

-- | Evaluate each action in the structure from left to right, and ignore the results.
-- For a version that doesn't ignore the results see 'sequence'.
sequence_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequence_ = Data.Foldable.sequenceA_
{-# INLINE sequence_ #-}

-- | Evaluate each action in the structure from left to right, and collect the results.
-- For a version that ignores the results see 'sequence_'.
sequence :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequence = Data.Traversable.sequenceA
{-# INLINE sequence #-}

-- | Construct a list of only the elements satisfying the provided predicate.
filter :: Foldable t => (a -> Bool) -> t a -> [a]
filter f = foldr (\a -> bool id (a:) (f a)) []
{-# INLINE[0] filter #-}
{-# RULES
"filter/List" [~1]
  filter = Prelude.filter
"filter/build" [1]
  forall (f :: a -> Bool) (as :: t a).
    filter f as = build (\k z -> foldr (\a -> bool id (k a) (f a)) z as)
  #-}

-- | Map over a list and filter out the 'Nothing' values.
filterMap :: Foldable t => (a -> Maybe b) -> t a -> [b]
filterMap f = foldr (\a -> maybe id (:) (f a)) []
{-# INLINE[0] filterMap #-}
{-# RULES
"filterMap/List" [~1]
  filterMap = Data.Maybe.mapMaybe
"filterMap/build" [1]
  forall (f :: a -> Maybe b) (as :: t a).
    filterMap f as = build (\k z -> foldr (\a -> maybe id k (f a)) z as)
  #-}

-- | A version of 'filter' in an 'Applicative' context.
filterA :: (Applicative f, Foldable t) => (a -> f Bool) -> t a -> f [a]
filterA f = foldr (\a as -> bool id (a:) <$> f a <*> as) (pure [])
{-# INLINE[1] filterA #-}
{-# RULES "filterA/List" filterA = Control.Monad.filterM #-}

-- | A version of 'filterMap' in an 'Applicative' context.
filterMapA :: (Applicative f, Foldable t) => (a -> f (Maybe b)) -> t a -> f [b]
filterMapA f = foldr (\a bs -> maybe id (:) <$> f a <*> bs) (pure [])
{-# INLINE filterMapA #-}

-- | Evaluate an expression to weak head normal form.
--
-- See 'Control.Exception.evaluate'.
evaluate :: MonadIO m => a -> m a
evaluate = liftIO . Control.Exception.evaluate
{-# INLINE evaluate #-}

-- | Strict version of '<$>'.
(<$!>) :: Functor f => (a -> b) -> f a -> f b
(<$!>) f = fmap (\a -> let b = f a in b `seq` b)
infixl 4 <$!>
{-# INLINE (<$!>) #-}

-- | Deeply strict version of '<$>'.
(<$!!>) :: (Functor f, NFData b) => (a -> b) -> f a -> f b
(<$!!>) f = fmap (\a -> let b = f a in b `deepseq` b)
infixl 4 <$!!>
{-# INLINE (<$!!>) #-}

-- | Read a value, returning 'Nothing' on failure.
tryRead :: Read a => String -> Maybe a
tryRead s = case [ a | (a, rest) <- reads s, null rest ] of
    [a] -> Just a
    _   -> Nothing

-- | Equivalent to @'trace' ('show' a) a@.
traceShow :: Show a => a -> a
traceShow a = Debug.Trace.trace (show a) a

-- | Equivalent to @'traceStack' ('show' a) a@
traceShowStack :: Show a => a -> a
traceShowStack a = Debug.Trace.traceStack (show a) a

-- | Like 'trace' but in the 'IO' monad, so it is sequenced with respect to other actions.
traceIO :: MonadIO m => String -> m ()
traceIO = liftIO . Debug.Trace.traceIO

-- | Like 'traceStack' but in the 'IO' monad, so it is sequenced with respect to other actions.
traceStackIO :: MonadIO m => String -> m ()
traceStackIO s = do
  traceIO s
  stack <- liftIO GHC.Stack.currentCallStack
  when (not (null stack)) $
    traceIO (GHC.Stack.renderStack stack)
