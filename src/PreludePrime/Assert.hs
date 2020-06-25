{-# LANGUAGE CPP #-}

-- | Unlike the @assert@ from "Control.Exception", these assertions are not controlled by the @-fignore-asserts@ GHC
-- flags, but rather are enabled or disabled by the @ignore-asserts@ flag that this package provides. The reason for
-- this is that the built-in @assert@ provided by "Control.Exception" isn't flexible; you can't pass a custom message,
-- and you can't define assert functions on top of it (such as 'assertM') because the location in the printed message
-- will be based on the call to @assert@ rather than the call to the function wrapping it. Using GHC's 'HasCallStack',
-- we can do better.
--
-- For assertions that you want to be unconditionally enabled, use @ensureXXX@ instead of @assertXXX@. This is best used
-- for cheap checks that are important to ensure program safety, such as checking that the index into an array is in
-- bounds.
--
-- Finally, these functions do not throw 'AssertionFailed' exceptions, but rather 'ErrorCall'. This is primarily because
-- 'ErrorCall' separates the error message from the source location, which can be convenient for logging. It also allows
-- them to be implemented in terms of 'error'.
--
-- The downside to using a package flag to disable assertions is a lack of granularity; assertions are enabled or
-- disabled for the entire program, whereas GHC's @-fignore-asserts@ flag can be controlled on a per-package or
-- per-module basis. There are some alternative implementations that could fix this, each with their own downsides:
--
-- * There is a hack which can determine whether assertions are enabled by passing "Control.Exception"'s @assert@ to
-- another function and using a rewrite rule for that function to determine whether GHC has replaced @assert@ with the
-- identity function. The problem is that GHC performs this substitution based on the module where @assert@ actually
-- appears in the source, so it requires the user of the custom assert function to pass @assert@ as an argument, which
-- is not great, ergonomically speaking.
--
-- * A custom GHC plugin could be used to perform the substitution of 'assertsEnabled' /after/ inlining has been
-- performed. This would introduce a lot of complexity, both because of implementing a GHC pass and due to the GHC API
-- changing between versions. It would also require the user to pass the @-fplugin=@ option to GHC in any project using
-- these assertions. Finally, assertions that get inlined into other modules would be controlled by the settings for
-- /that/ module rather than the module in which they were originially placed.
--
-- * An API or utility could be provided making it easy for individual packages to define their own @Assert@ modules
-- controlled by flags specific to that package (e.g, the @vector@ package has multiple flags controlling different
-- kinds of assertions). However, this sort of thing seems out of scope for this package which is intended for
-- convenience, not as a framework which requires additional setup and boilerplate.
module PreludePrime.Assert
( -- * Unconditional Assertions
  -- | These assertions are always checked.
  ensure
, ensureMsg
, ensurePred
, ensurePredMsg
, ensureIO
, ensureMsgIO
, ensureM
, ensureMsgM

-- * Conditional Assertions
-- | These assertions are only checked when the @ignore-asserts@ package flag is disabled.
, assert
, assertMsg
, assertPred
, assertPredMsg
, assertIO
, assertMsgIO
, assertM
, assertMsgM

-- * Conditions
, ifAssertsEnabled
, assertsEnabled
) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import PreludePrime.Exception

-- [NOTE: withFrozenCallStack]
--
-- The reason for the 'withFrozenCallStack' calls before every error call is so that the callstack doesn't spuriously
-- include the source location from this module, and instead ends at the location of the call to 'assert', etc.

-- | Call @'error' "Assertion failed"@ if the provided condition is 'False'.
ensure :: HasCallStack => Bool -> a -> a
ensure cond a = if cond then a else withFrozenCallStack $ error "Assertion failed"

-- | Like 'ensure', but with the ability to provide a custom error message.
ensureMsg :: HasCallStack => Bool -> String -> a -> a
ensureMsg cond msg a = if cond then a else withFrozenCallStack $ error msg

-- | Like 'ensure', but with a predicate to test: @ensurePred p a = 'ensure' (p a) a@
ensurePred :: HasCallStack => (a -> Bool) -> a -> a
ensurePred f a = if f a then a else withFrozenCallStack $ error "Assertion failed"

-- | Like 'ensureMsg', but with a predicate to test: @ensurePredMsg p s a = 'ensureMsg' (p a) s a@
ensurePredMsg :: HasCallStack => (a -> Bool) -> String -> a -> a
ensurePredMsg f msg a = if f a then a else withFrozenCallStack $ error msg

-- | Like 'ensure', but is sequenced with respect to other IO actions.
ensureIO :: (HasCallStack, MonadIO m) => Bool -> m ()
ensureIO cond = unless cond $ withFrozenCallStack $ errorIO "Assertion failed"

-- | Like 'ensureMsg', but is sequenced with respect to other IO actions.
ensureMsgIO :: (HasCallStack, MonadIO m) => Bool -> String -> m ()
ensureMsgIO cond msg = unless cond $ withFrozenCallStack $ errorIO msg

-- | 'ensureIO' generalized to 'MonadThrow'.
ensureM :: (HasCallStack, MonadThrow m) => Bool -> m ()
ensureM cond = unless cond $ withFrozenCallStack $ errorM "Assertion failed"

-- | 'ensureMsgIO' generalized to 'MonadThrow'.
ensureMsgM :: (HasCallStack, MonadThrow m) => Bool -> String -> m ()
ensureMsgM cond msg = unless cond $ withFrozenCallStack $ errorM msg

-- [NOTE: inlining]
--
-- The assert functions are all inlined to ensure that all code relating to checking the assertions condition is removed
-- by the optimizer when assertions are disabled.

-- | Like 'ensure', but can be disabled via this package's @ignore-asserts@ flag.
assert :: HasCallStack => Bool -> a -> a
assert cond a = ifAssertsEnabled (withFrozenCallStack $ ensure cond a) a
{-# INLINE assert #-}

-- | Like 'ensureMsg', but can be disabled via this package's @ignore-asserts@ flag.
assertMsg :: HasCallStack => Bool -> String -> a -> a
assertMsg cond msg a = ifAssertsEnabled (withFrozenCallStack $ ensureMsg cond msg a) a
{-# INLINE assertMsg #-}

-- | Like 'ensurePred', but can be disabled via this package's @ignore-asserts@ flag.
assertPred :: HasCallStack => (a -> Bool) -> a -> a
assertPred p a = ifAssertsEnabled (withFrozenCallStack $ ensurePred p a) a
{-# INLINE assertPred #-}

-- | Like 'ensurePredMsg', but can be disabled via this package's @ignore-asserts@ flag.
assertPredMsg :: HasCallStack => (a -> Bool) -> String -> a -> a
assertPredMsg p s a = ifAssertsEnabled (withFrozenCallStack $ ensurePredMsg p s a) a
{-# INLINE assertPredMsg #-}

-- | Like 'ensureIO', but can be disabled via this package's @ignore-asserts@ flag.
assertIO :: (HasCallStack, MonadIO m) => Bool -> m ()
assertIO cond = ifAssertsEnabled (withFrozenCallStack $ ensureIO cond) (pure ())
{-# INLINE assertIO #-}

-- | Like 'ensureMsgIO', but can be disabled via this package's @ignore-asserts@ flag.
assertMsgIO :: (HasCallStack, MonadIO m) => Bool -> String -> m ()
assertMsgIO cond msg = ifAssertsEnabled (withFrozenCallStack $ ensureMsgIO cond msg) (pure ())
{-# INLINE assertMsgIO #-}

-- | Like 'ensureM', but can be disabled via this package's @ignore-asserts@ flag.
assertM :: (HasCallStack, MonadThrow m) => Bool -> m ()
assertM cond = ifAssertsEnabled (withFrozenCallStack $ ensureM cond) (pure ())
{-# INLINE assertM #-}

-- | Like 'ensureMsgM', but can be disabled via this package's @ignore-asserts@ flag.
assertMsgM :: (HasCallStack, MonadThrow m) => Bool -> String -> m ()
assertMsgM cond msg = ifAssertsEnabled (withFrozenCallStack $ ensureMsgM cond msg) (pure ())
{-# INLINE assertMsgM #-}

-- | @ifAssertsEnabled a b = if 'assertsEnabled' then a else b@.
ifAssertsEnabled :: a -> a -> a
#if !PRELUDE_PRIME_IGNORE_ASSERTS
ifAssertsEnabled a _ = a
#else
ifAssertsEnabled _ a = a
#endif
{-# INLINE ifAssertsEnabled #-}

-- | True unless the @ignore-asserts@ flag is enabled.
assertsEnabled :: Bool
assertsEnabled = ifAssertsEnabled True False
