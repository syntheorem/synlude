{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}

-- | An alternative prelude for modern Haskell.
module Synlude
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
, Id
, pattern Id
, unId

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
, Control.Monad.Monad((>>=), (>>))
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
, Data.Foldable.Foldable(fold, foldMap, foldr, foldr', foldl, foldl')

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
, Data.Foldable.concat
, Data.Foldable.concatMap
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

-- * Forcing evaluation
, evaluate
, Prelude.seq
, (Prelude.$!)
, (Control.Monad.<$!>)
, Control.DeepSeq.NFData(rnf)
, Control.DeepSeq.deepseq
, Control.DeepSeq.force
, (Control.DeepSeq.$!!)
, (Control.DeepSeq.<$!!>)

-- * Exceptions
, Synlude.Exception.undefined
, Synlude.Exception.error
, Synlude.Exception.errorIO
, Synlude.Exception.errorM
, Synlude.Exception.throw
, Synlude.Exception.throwIO
, Synlude.Exception.throwM
, Synlude.Exception.Exception(toException, fromException, displayException)
, Synlude.Exception.SomeException(SomeException)
, Synlude.Exception.Partial

-- ** Assertions
-- | See "Synlude.Assert" for additional documentation.
, Synlude.Assert.assert
, Synlude.Assert.assertMsg
, Synlude.Assert.assertPred
, Synlude.Assert.assertPredMsg
, Synlude.Assert.assertIO
, Synlude.Assert.assertMsgIO
, Synlude.Assert.assertM
, Synlude.Assert.assertMsgM
, Synlude.Assert.ensure
, Synlude.Assert.ensureMsg
, Synlude.Assert.ensurePred
, Synlude.Assert.ensurePredMsg
, Synlude.Assert.ensureIO
, Synlude.Assert.ensureMsgIO
, Synlude.Assert.ensureM
, Synlude.Assert.ensureMsgM

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

-- * Additional types
, Data.Void.Void
, Data.Proxy.Proxy(Proxy)
, GHC.Generics.Generic -- included for easy deriving

-- * Tracing
-- | Provided in the prelude to avoid having to add and remove @import "Debug.Trace"@ statements
-- when debugging.
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
import qualified Data.Int
import qualified Data.Kind
import qualified Data.Maybe
import qualified Data.Monoid
import qualified Data.Ord
import qualified Data.Semigroup
import qualified Data.String
import qualified Data.Traversable
import qualified Data.Tuple
import qualified Data.Typeable
import qualified Data.Proxy
import qualified Data.Word
import qualified Data.Void
import qualified Debug.Trace
import qualified GHC.Generics
import qualified GHC.Stack
import qualified Numeric.Natural
import qualified Prelude
import qualified Synlude.Assert
import qualified Synlude.Exception
import qualified System.IO
import qualified Text.Read
import qualified Text.Show

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor.Identity (Identity(Identity, runIdentity))

import Prelude
  ( Bool, Int, String, Maybe(Just, Nothing), Functor, Applicative
  , Read, reads, null, Show(show), (.), not, (&&), (||), ($)
  )

-- | A lifted version of '&&'.
--
-- Useful when combined with the 'Applicative' instance for functions,
-- e.g. @(isSpace \<&&\> isControl)  c@.
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

-- | An alias for 'fmap'.
--
-- In an ideal world, there would be no @fmap@, only @map@. But instead, the @map@ from "Prelude" is
-- specialized to lists. This alias partially rectifices that by allowing you to use @map@ with any
-- 'Functor'.
map :: Functor f => (a -> b) -> f a -> f b
map = Data.Functor.fmap
{-# INLINE map #-}

-- | An alias for 'Identity'.
--
-- In the same way that the identity function is abbreviated to 'id', the identity functor should be
-- abbreviated to @Id@ to reduce the level of visual noise when it is heavily used.
type Id = Identity

-- | An alias for the 'Identity' constructor.
pattern Id :: a -> Id a
pattern Id a = Identity a
{-# COMPLETE Id #-}

-- | An alias for 'runIdentity'.
unId :: Id a -> a
unId = runIdentity
{-# INLINE unId #-}

-- | An alias for 'Control.Monad.replicateM'.
--
-- After 'Applicative' was made a superclass of 'Monad', the constraint for
-- 'Control.Monad.replicateM' was relaxed to 'Applicative', so this renaming better reflects the
-- current constraint.
replicateA :: Applicative f => Int -> f a -> f [a]
replicateA = Control.Monad.replicateM
{-# INLINE replicateA #-}

-- | An alias for 'Control.Monad.replicateM_'.
--
-- After 'Applicative' was made a superclass of 'Monad', the constraint for
-- 'Control.Monad.replicateM_' was relaxed to 'Applicative', so this renaming better reflects the
-- current constraint.
replicateA_ :: Applicative f => Int -> f a -> f ()
replicateA_ = Control.Monad.replicateM_
{-# INLINE replicateA_ #-}

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
