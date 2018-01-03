-- | Additional exception handling functions from "Control.Exception".
--
-- Does not include functions or types that are already in "PreludePrime".
module PreludePrime.Exception
( Control.Exception.throwTo
, Control.Exception.catch
, Control.Exception.catches
, Control.Exception.Handler(..)
, Control.Exception.catchJust
, Control.Exception.handle
, Control.Exception.handleJust
, Control.Exception.try
, Control.Exception.tryJust
, mapException
, Control.Exception.mask
, Control.Exception.mask_
, Control.Exception.uninterruptibleMask
, Control.Exception.uninterruptibleMask_
, Control.Exception.MaskingState(..)
, Control.Exception.interruptible
, Control.Exception.allowInterrupt
, Control.Exception.bracket
, Control.Exception.bracket_
, Control.Exception.bracketOnError
, Control.Exception.finally
, Control.Exception.onException
) where

import Control.Exception (Exception)
import Data.Function (flip)

import qualified Control.Exception

-- | Map one exception onto another.
--
-- Note that this function has its arguments flipped compared to 'Control.Exception.mapException'.
-- This is because I really feel that it should match the order of 'catch' and friends.
mapException :: (Exception e1, Exception e2) => a -> (e1 -> e2) -> a
mapException = flip Control.Exception.mapException
{-# INLINE mapException #-}
