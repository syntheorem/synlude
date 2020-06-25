{-# LANGUAGE NoImplicitPrelude #-}

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

-- * Tuples
, Data.Tuple.fst
, Data.Tuple.snd
, Data.Tuple.curry
, Data.Tuple.uncurry

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

-- * Char and String
, Data.Char.Char
, Data.String.String
, Text.Show.Show(show)
, Text.Read.Read
, tryRead

-- * Numeric

-- ** Basic types
, Prelude.Int
, Prelude.Word
, Prelude.Float
, Prelude.Double

-- ** Arbitrary precision types
, Prelude.Integer
, Numeric.Natural.Natural
, Prelude.Rational
, (Data.Ratio.%)

-- ** Explicit precision types
, Data.Int.Int8
, Data.Int.Int16
, Data.Int.Int32
, Data.Int.Int64
, Data.Word.Word8
, Data.Word.Word16
, Data.Word.Word32
, Data.Word.Word64

-- ** @class Num@
, Prelude.Num((+), (-), (*), negate, abs, signum, fromInteger)
, Prelude.subtract

-- ** @class Real@
, Prelude.Real(toRational)

-- ** @class Integral@
, Prelude.Integral(quot, rem, div, mod, quotRem, divMod, toInteger)
, Prelude.even
, Prelude.odd
, Prelude.gcd
, Prelude.lcm
, (Prelude.^)
, Prelude.fromIntegral

-- ** @class Fractional@
, Prelude.Fractional((/), recip, fromRational)
, Prelude.realToFrac
, (Prelude.^^)

-- ** @class RealFrac@
, Prelude.RealFrac(properFraction, truncate, round, ceiling, floor)

-- * Enumerations
-- primarily included because these classes aren't exported anywhere else.
, Prelude.Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen, enumFromTo, enumFromThenTo)
, Prelude.Bounded(minBound, maxBound)

-- * Semigroup and Monoid
, Data.Semigroup.Semigroup((<>))
, Data.Monoid.Monoid(mempty, mconcat)

-- * Functor
, Data.Functor.Functor(fmap)
, map
, (Data.Functor.<$>)
, (Data.Functor.<$)
, (Data.Functor.$>)
, (Data.Functor.<&>)
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
, Control.Monad.Fix.MonadFix(mfix)
, (Control.Monad.=<<)
, (Control.Monad.>=>)
, (Control.Monad.<=<)
, Control.Monad.join

-- * Alternative
, Control.Applicative.Alternative(empty, (<|>), some, many)
, Control.Applicative.optional
, Control.Monad.guard

-- * Foldable
, Data.Foldable.Foldable (fold, foldMap, foldr, foldr', foldl, foldl')

-- ** Applicative folds
, Data.Foldable.traverse_
, Data.Foldable.for_
, Data.Foldable.sequenceA_
, Data.Foldable.asum

-- ** Monadic folds
, Data.Foldable.foldlM
, Data.Foldable.foldrM
, Data.Foldable.mapM_
, Data.Foldable.forM_
, Data.Foldable.sequence_

-- ** Specialized folds
, Data.Foldable.toList
, Data.Foldable.length
, Data.Foldable.null
, Data.Foldable.elem
, Data.Foldable.find
, Data.Foldable.and
, Data.Foldable.or
, Data.Foldable.all
, Data.Foldable.any
, Data.Foldable.sum
, Data.Foldable.product
, Data.Foldable.maximum
, Data.Foldable.minimum
, Data.Foldable.maximumBy
, Data.Foldable.minimumBy

-- * Traversable
, Data.Traversable.Traversable(traverse, sequenceA, mapM, sequence)
, Data.Traversable.for
, Data.Traversable.forM

-- * Filterable
-- | Abstraction of data structures which can be filtered, from "Data.Witherable.Class".
, Data.Witherable.Class.Filterable
, Data.Witherable.Class.filter
, filterMap
, Data.Witherable.Class.Witherable
, Data.Witherable.Class.filterA
, filterMapA
, filterMapM

-- * Forcing evaluation
, evaluate
, Prelude.seq
, (Prelude.$!)
, (Control.Monad.<$!>)
, Control.DeepSeq.NFData
, Control.DeepSeq.deepseq
, Control.DeepSeq.force
, (Control.DeepSeq.$!!)
, (Control.DeepSeq.<$!!>)

-- * Exceptions
, PreludePrime.Exception.undefined
, PreludePrime.Exception.error
, PreludePrime.Exception.errorIO
, PreludePrime.Exception.errorM
, PreludePrime.Exception.throw
, PreludePrime.Exception.throwIO
, PreludePrime.Exception.throwM
, PreludePrime.Exception.Exception(toException, fromException, displayException)
, PreludePrime.Exception.SomeException(SomeException)
, PreludePrime.Exception.Partial

-- ** Assertions
-- | See "PreludePrime.Assert" for additional documentation.
, PreludePrime.Assert.assert
, PreludePrime.Assert.assertMsg
, PreludePrime.Assert.assertPred
, PreludePrime.Assert.assertPredMsg
, PreludePrime.Assert.assertIO
, PreludePrime.Assert.assertMsgIO
, PreludePrime.Assert.assertM
, PreludePrime.Assert.assertMsgM
, PreludePrime.Assert.ensure
, PreludePrime.Assert.ensureMsg
, PreludePrime.Assert.ensurePred
, PreludePrime.Assert.ensurePredMsg
, PreludePrime.Assert.ensureIO
, PreludePrime.Assert.ensureMsgIO
, PreludePrime.Assert.ensureM
, PreludePrime.Assert.ensureMsgM

-- * IO
-- | Functions which do actual IO should be imported from "System.IO".
, System.IO.IO
, Control.Monad.IO.Class.MonadIO(liftIO)

-- * Type casts and coercions
, Data.Typeable.Typeable
, Data.Coerce.Coercible
, Data.Coerce.coerce

-- * Kinds
, Data.Kind.Type
, Data.Kind.Constraint
, GHC.TypeLits.Nat
, GHC.TypeLits.Symbol

-- * Additional types
, Data.Void.Void
, Data.Proxy.Proxy(Proxy)
, GHC.Generics.Generic -- included for easy deriving

-- * Tracing
-- | Provided in the prelude to avoid having to add and remove @import "Debug.Trace"@ statements when debugging.
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
import qualified Control.Monad.Fix
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
import qualified Data.Kind
import qualified Data.Maybe
import qualified Data.Monoid
import qualified Data.Ord
import qualified Data.Ratio
import qualified Data.Semigroup
import qualified Data.String
import qualified Data.Traversable
import qualified Data.Tuple
import qualified Data.Typeable
import qualified Data.Proxy
import qualified Data.Witherable.Class
import qualified Data.Word
import qualified Data.Void
import qualified Debug.Trace
import qualified GHC.Generics
import qualified GHC.Stack
import qualified GHC.TypeLits
import qualified Numeric.Natural
import qualified Prelude
import qualified PreludePrime.Assert
import qualified PreludePrime.Exception
import qualified System.IO
import qualified Text.Read
import qualified Text.Show

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Witherable.Class (Filterable, Witherable)

import Prelude
  ( Bool, Int, String, Maybe(Just, Nothing), Functor, Applicative
  , Monad, Read, reads, null, Show(show), (.), not, (&&), (||), ($)
  )

-- | A lifted version of '&&'.
--
-- Useful when combined with the 'Applicative' instance for functions, e.g. @(isSpace \<&&\> isControl) c@.
(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = Control.Applicative.liftA2 (&&)
infixr 3 <&&>
{-# INLINE (<&&>) #-}

-- | A lifted version of '||'.
--
-- Useful when combined with the 'Applicative' instance for functions, e.g. @(isSpace \<||\> isControl) c@.
(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = Control.Applicative.liftA2 (||)
infixr 2 <||>
{-# INLINE (<||>) #-}

-- | An alias for 'fmap'.
--
-- In an ideal world, there would be no @fmap@, only @map@. But instead, the @map@ from "Prelude" is specialized to
-- lists. This alias partially rectifices that by allowing you to use @map@ with any 'Functor'.
map :: Functor f => (a -> b) -> f a -> f b
map = Data.Functor.fmap
{-# INLINE map #-}

-- | An alias for 'Control.Monad.replicateM'.
--
-- After 'Applicative' was made a superclass of 'Monad', the constraint for 'Control.Monad.replicateM' was relaxed to
-- 'Applicative', so this renaming better reflects the current constraint.
replicateA :: Applicative f => Int -> f a -> f [a]
replicateA = Control.Monad.replicateM
{-# INLINE replicateA #-}

-- | An alias for 'Control.Monad.replicateM_'.
--
-- After 'Applicative' was made a superclass of 'Monad', the constraint for 'Control.Monad.replicateM_' was relaxed to
-- 'Applicative', so this renaming better reflects the current constraint.
replicateA_ :: Applicative f => Int -> f a -> f ()
replicateA_ = Control.Monad.replicateM_
{-# INLINE replicateA_ #-}

-- | An alias for 'Data.Witherable.Class.mapMaybe'.
--
-- Honestly, I just dislike the name @mapMaybe@ because it makes me think of 'fmap' specialized to 'Maybe', whereas the
-- name @filterMap@ clearly indicates simultaneously filtering and mapping the values in some structure.
filterMap :: Filterable t => (a -> Maybe b) -> t a -> t b
filterMap = Data.Witherable.Class.mapMaybe
{-# INLINE filterMap #-}

-- | An alias for 'Data.Witherable.Class.wither'.
--
-- Similarly to 'filterMap', I find this name much more clear than 'wither'.
filterMapA :: (Applicative f, Witherable t) => (a -> f (Maybe b)) -> t a -> f (t b)
filterMapA = Data.Witherable.Class.wither
{-# INLINE filterMapA #-}

-- | An alias for 'Data.Witherable.Class.witherM'.
--
-- Similarly to 'filterMap', I find this name much more clear than 'witherM'.
filterMapM :: (Monad m, Witherable t) => (a -> m (Maybe b)) -> t a -> m (t b)
filterMapM = Data.Witherable.Class.witherM
{-# INLINE filterMapM #-}

-- | Lifted version of 'Control.Exception.evaluate'.
evaluate :: MonadIO m => a -> m a
evaluate = liftIO . Control.Exception.evaluate
{-# INLINE evaluate #-}

-- | Parse a value using 'Text.Read.reads', returning 'Nothing' on failure.
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
traceStackIO s = liftIO $ do
  traceIO s
  stack <- GHC.Stack.currentCallStack
  when (not (null stack)) $
    traceIO (GHC.Stack.renderStack stack)
