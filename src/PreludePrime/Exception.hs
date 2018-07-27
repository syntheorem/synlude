{-# LANGUAGE CPP, TypeApplications #-}

-- | Replacement for "Control.Exception".
--
-- This module is mostly a drop-in replacement for "Control.Exception", with two
-- key differences:
--
-- * The functions are generalized from the 'IO' monad to any monad which supports
-- the requisite operations. This is done using 'MonadThrow', 'MonadCatch', and
-- 'MonadMask' from "Control.Monad.Catch" in the @exceptions@ package. In fact, most
-- of this module consists of re-exports from that package.
--
-- * Several extra utility functions are included; in particular, better assertions
-- and functions for dealing with asynchronous exceptions.
--
-- * All of the monadic throwing functions have an @xxxIO@ and @xxxM@ variant. This
-- is for convenience when your constraints include 'MonadIO' but not 'MonadThrow'.
module PreludePrime.Exception
( Control.Exception.Exception(toException, fromException, displayException)
, Control.Exception.SomeException(..)

-- * Throwing exceptions
, Control.Exception.throw
, throwIO
, Control.Monad.Catch.MonadThrow(throwM)

-- ** Errors
, Prelude.undefined
, Prelude.error
, errorIO
, errorM

-- ** Assertions
--
-- | Unlike the @assert@ from "Control.Exception", these assertions are not controlled
-- by @-fignore-asserts@, but rather are enabled or disabled by the @ignore-asserts@ flag
-- that this package provides. The reason for this is that the built-in @assert@ provided
-- by "Control.Exception" isn't flexible; you can't pass a custom message, and you can't
-- define assert functions on top of it (such as 'assertM') because the location in the
-- printed message will be based on the call to @assert@ rather than the call to the
-- function wrapping it. Using GHC's 'HasCallStack', we can do better.
--
-- For assertions that you want to be unconditionally enabled, use @ensureXXX@ instead of
-- @assertXXX@. This is best used for cheap checks that are important to ensure program safety,
-- such as checking that the index into an array is in bounds. Note though that best practice
-- is to throw a custom exception instead, allowing users to potentially catch it and deal with
-- the specific error. However, using 'ensure' is usually more convenient while developing.
--
-- Finally, these functions do not throw 'AssertionFailed' exceptions, but rather 'ErrorCall'.
-- This is primarily because 'ErrorCall' separates the error message from the source location,
-- which can be convenient for logging. It also allows them to be implemented in terms of 'error'.
, assert
, assertMsg
, assertIO
, assertMsgIO
, assertM
, assertMsgM
, ensure
, ensureMsg
, ensureIO
, ensureMsgIO
, ensureM
, ensureMsgM

-- * Catching exceptions
, Control.Monad.Catch.MonadCatch(catch)
, Control.Monad.Catch.Handler(..)
, Control.Monad.Catch.catches
, Control.Monad.Catch.catchIf
, Control.Monad.Catch.catchJust
, Control.Monad.Catch.catchAll
, Control.Monad.Catch.handle
, Control.Monad.Catch.handleIf
, Control.Monad.Catch.handleJust
, Control.Monad.Catch.handleAll
, Control.Monad.Catch.try
, tryIf
, Control.Monad.Catch.tryJust
, tryAll

-- * Mapping exceptions
, Control.Exception.mapException
, mapExceptionM

-- * Asynchronous exceptions
, throwTo
, Control.Exception.SomeAsyncException(..)
, Control.Exception.asyncExceptionToException
, Control.Exception.asyncExceptionFromException
, isAsyncException

-- ** Masking asynchronous exceptions
, Control.Monad.Catch.MonadMask(mask, uninterruptibleMask)
, Control.Monad.Catch.mask_
, Control.Monad.Catch.uninterruptibleMask_
, Control.Exception.MaskingState(..)
, getMaskingState
, allowInterrupt
, interruptible

-- ** Catching only synchronous exceptions
-- | Versions of the catch functions which don't catch asynchronous exceptions.
-- See 'isAsyncException' for more details on which exceptions are considered asynchronous.
, catchSync
, catchSyncIf
, catchSyncJust
, catchSyncAll
, handleSync
, handleSyncIf
, handleSyncJust
, handleSyncAll
, trySync
, trySyncIf
, trySyncJust
, trySyncAll

-- * Running cleanup actions
, Control.Monad.Catch.finally
, Control.Monad.Catch.onException
, Control.Monad.Catch.onError
, Control.Monad.Catch.bracket
, Control.Monad.Catch.bracket_
, Control.Monad.Catch.bracketOnError

-- * Exception data types
, Control.Exception.AllocationLimitExceeded(..)
, Control.Exception.ArithException(..)
, Control.Exception.ArrayException(..)
, Control.Exception.AssertionFailed(..)
, Control.Exception.AsyncException(..)
, Control.Exception.BlockedIndefinitelyOnMVar(..)
, Control.Exception.BlockedIndefinitelyOnSTM(..)
, Control.Exception.CompactionFailed(..)
, Control.Exception.Deadlock(..)
, Control.Exception.ErrorCall(..)
, Control.Exception.IOException
, Control.Exception.NestedAtomically(..)
, Control.Exception.NoMethodError(..)
, Control.Exception.NonTermination(..)
, Control.Exception.PatternMatchFail(..)
, Control.Exception.RecConError(..)
, Control.Exception.RecSelError(..)
, Control.Exception.RecUpdError(..)
, Control.Exception.TypeError(..)
) where

import qualified Control.Exception
import Control.Monad.Catch

import Prelude
import Control.Concurrent (ThreadId)
import Control.Exception (MaskingState, SomeAsyncException)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO(withRunInIO))
import Data.Maybe (isJust)
import GHC.Exception (errorCallWithCallStackException)
import GHC.Stack (HasCallStack, withFrozenCallStack, callStack)

-- | Lifted version of "Control.Exception"'s 'Control.Exception.throwIO'.
throwIO :: (MonadIO m, Exception e) => e -> m a
throwIO = liftIO . Control.Exception.throwIO

-- | Like 'error', but is sequenced with respect to other IO actions.
-- See 'Control.Exception.throwIO' for why this is necessary.
errorIO :: (HasCallStack, MonadIO m) => String -> m a
errorIO s = throwIO (errorCallWithCallStackException s callStack)

-- | 'errorIO' generalized to 'MonadThrow'.
errorM :: (HasCallStack, MonadThrow m) => String -> m a
errorM s = throwM (errorCallWithCallStackException s callStack)

-- | Call @'error' "Assertion failed"@ if the provided condition is 'False'.
ensure :: HasCallStack => Bool -> a -> a
ensure b a = withFrozenCallStack $ ensureMsg b "Assertion failed" a

-- | Like 'ensure', but with the ability to provide a custom error message.
ensureMsg :: HasCallStack => Bool -> String -> a -> a
ensureMsg True  _ a = a
ensureMsg False s _ = withFrozenCallStack $ error s

-- | Like 'ensure', but is sequenced with respect to other IO actions.
ensureIO :: (HasCallStack, MonadIO m) => Bool -> m ()
ensureIO b = withFrozenCallStack $ ensureMsgIO b "Assertion failed"

-- | Like 'ensureMsg', but is sequenced with respect to other IO actions.
ensureMsgIO :: (HasCallStack, MonadIO m) => Bool -> String -> m ()
ensureMsgIO True  _ = pure ()
ensureMsgIO False s = withFrozenCallStack $ errorIO s

-- | 'ensureIO' generalized to 'MonadThrow'.
ensureM :: (HasCallStack, MonadThrow m) => Bool -> m ()
ensureM b = withFrozenCallStack $ ensureMsgM b "Assertion failed"

-- | 'ensureMsgIO' generalized to 'MonadThrow'.
ensureMsgM :: (HasCallStack, MonadThrow m) => Bool -> String -> m ()
ensureMsgM True  _ = pure ()
ensureMsgM False s = withFrozenCallStack $ errorM s

-- | Like 'ensure', but can be disabled via this package's @ignore-asserts@ flag.
#if ASSERT
assert :: HasCallStack => Bool -> a -> a
assert b a = withFrozenCallStack $ ensure b a
#else
assert :: Bool -> a -> a
assert _ a = a
#endif
{-# INLINE assert #-}

-- | Like 'ensureMsg', but can be disabled via this package's @ignore-asserts@ flag.
#if ASSERT
assertMsg :: HasCallStack => Bool -> String -> a -> a
assertMsg b msg a = withFrozenCallStack $ ensureMsg b msg a
#else
assertMsg :: Bool -> String -> a -> a
assertMsg _ _ a = a
#endif
{-# INLINE assertMsg #-}

-- | Like 'ensureIO', but can be disabled via this package's @ignore-asserts@ flag.
#if ASSERT
assertIO :: (HasCallStack, MonadIO m) => Bool -> m ()
assertIO b = withFrozenCallStack $ ensureIO b
#else
assertIO :: MonadIO m => Bool -> m ()
assertIO _ = pure ()
#endif
{-# INLINE assertIO #-}

-- | Like 'ensureMsgIO', but can be disabled via this package's @ignore-asserts@ flag.
#if ASSERT
assertMsgIO :: (HasCallStack, MonadIO m) => Bool -> String -> m ()
assertMsgIO b msg = withFrozenCallStack $ ensureMsgIO b msg
#else
assertMsgIO :: MonadIO m => Bool -> String -> m ()
assertMsgIO _ _ = pure ()
#endif
{-# INLINE assertMsgIO #-}

-- | Like 'ensureM', but can be disabled via this package's @ignore-asserts@ flag.
#if ASSERT
assertM :: (HasCallStack, MonadThrow m) => Bool -> m ()
assertM b = withFrozenCallStack $ ensureM b
#else
assertM :: MonadThrow m => Bool -> m ()
assertM _ = pure ()
#endif
{-# INLINE assertM #-}

-- | Like 'ensureMsgM', but can be disabled via this package's @ignore-asserts@ flag.
#if ASSERT
assertMsgM :: (HasCallStack, MonadThrow m) => Bool -> String -> m ()
assertMsgM b msg = withFrozenCallStack $ ensureMsgM b msg
#else
assertMsgM :: MonadThrow m => Bool -> String -> m ()
assertMsgM _ _ = pure ()
#endif
{-# INLINE assertMsgM #-}

-- | Lifted version of "Control.Exception"'s 'Control.Exception.throwTo'.
throwTo :: (MonadIO m, Exception e) => ThreadId -> e -> m ()
throwTo tid e = liftIO $ Control.Exception.throwTo tid e

-- | Predicate to test if an exception is asynchronous.
--
-- It's important to note that this is determined by the type of the exception; specifically,
-- whether the type uses 'asyncExceptionToException' as its implementation for 'toException'.
-- Since 'throw' and 'throwTo' don't constrain the types of exceptions thrown, this function
-- does not prove whether the given exception was actually thrown asynchronously or not.
--
-- It is therefore recommended to only use 'throw' for synchronous exception types and only
-- use 'throwTo' for asynchronous exception types.
isAsyncException :: Exception e => e -> Bool
isAsyncException = isJust . fromException @SomeAsyncException . toException

-- | A variant of 'try' which takes a boolean predicate to determine which exceptions are caught.
tryIf :: (MonadCatch m, Exception e) => (e -> Bool) -> m a -> m (Either e a)
tryIf f = tryJust (\e -> if f e then Just e else Nothing)

-- | A variant of 'try' which catches all exceptions.
tryAll :: MonadCatch m => m a -> m (Either SomeException a)
tryAll = try

-- | Like 'mapException', but maps exceptions thrown by a monadic action.
mapExceptionM :: (Exception e1, Exception e2, MonadCatch m) => (e1 -> e2) -> m a -> m a
mapExceptionM f action = action `catch` (\e -> throwM (f e))

-- | Lifted version of "Control.Exception"'s 'Control.Exception.getMaskingState'.
getMaskingState :: MonadIO m => m MaskingState
getMaskingState = liftIO Control.Exception.getMaskingState

-- | Lifted version of "Control.Exception"'s 'Control.Exception.allowInterrupt'.
allowInterrupt :: MonadIO m => m ()
allowInterrupt = liftIO Control.Exception.allowInterrupt

-- | Lifted version of "Control.Exception"'s 'Control.Exception.interruptible'.
interruptible :: MonadUnliftIO m => m a -> m a
interruptible action = withRunInIO ($ action)

-- | Like 'catch', but doesn't catch asynchronous exceptions.
catchSync :: (MonadCatch m, Exception e) => m a -> (e -> m a) -> m a
catchSync = catchIf (not . isAsyncException)

-- | Like 'catchIf', but doesn't catch asynchronous exceptions.
catchSyncIf :: (MonadCatch m, Exception e) => (e -> Bool) -> m a -> (e -> m a) -> m a
catchSyncIf f = catchIf (\e -> not (isAsyncException e) && f e)

-- | Like 'catchJust', but doesn't catch asynchronous exceptions.
catchSyncJust :: (MonadCatch m, Exception e) => (e -> Maybe b) -> m a -> (b -> m a) -> m a
catchSyncJust f = catchJust (\e -> if isAsyncException e then Nothing else f e)

-- | Like 'catchAll', but doesn't catch asynchronous exceptions.
catchSyncAll :: MonadCatch m => m a -> (SomeException -> m a) -> m a
catchSyncAll = catchSync

-- | Like 'catch', but doesn't catch asynchronous exceptions.
handleSync :: (MonadCatch m, Exception e) => (e -> m a) -> m a -> m a
handleSync = flip catchSync

-- | Like 'handleIf', but doesn't catch asynchronous exceptions.
handleSyncIf :: (MonadCatch m, Exception e) => (e -> Bool) -> (e -> m a) -> m a -> m a
handleSyncIf f = flip (catchSyncIf f)

-- | Like 'handleJust', but doesn't catch asynchronous exceptions.
handleSyncJust :: (MonadCatch m, Exception e) => (e -> Maybe b) -> (b -> m a) -> m a -> m a
handleSyncJust f = flip (catchSyncJust f)

-- | Like 'handleAll', but doesn't catch asynchronous exceptions.
handleSyncAll :: MonadCatch m => (SomeException -> m a) -> m a -> m a
handleSyncAll = flip catchSyncAll

-- | Like 'try', but doesn't catch asynchronous exceptions.
trySync :: (MonadCatch m, Exception e) => m a -> m (Either e a)
trySync = tryIf (not . isAsyncException)

-- | Like 'tryIf', but doesn't catch asynchronous exceptions.
trySyncIf :: (MonadCatch m, Exception e) => (e -> Bool) -> m a -> m (Either e a)
trySyncIf f = tryIf (\e -> not (isAsyncException e) && f e)

-- | Like 'tryJust', but doesn't catch asynchronous exceptions.
trySyncJust :: (MonadCatch m, Exception e) => (e -> Maybe b) -> m a -> m (Either b a)
trySyncJust f = tryJust (\e -> if isAsyncException e then Nothing else f e)

-- | Like 'tryAll', but doesn't catch asynchronous exceptions.
trySyncAll :: MonadCatch m => m a -> m (Either SomeException a)
trySyncAll = trySync
