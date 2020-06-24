-- | Module which re-exports all numeric types and functions from @base@.
module PreludePrime.Numeric
(
-- * Prelude re-exports
  Prelude.Int
, Prelude.Integer
, Prelude.Float
, Prelude.Double
, Prelude.Word
, Prelude.Num(..)
, Prelude.Real(..)
, Prelude.Integral(..)
, Prelude.Fractional(..)
, Prelude.Floating(..)
, Prelude.RealFrac(..)
, Prelude.RealFloat(..)
, Prelude.subtract
, Prelude.even
, Prelude.odd
, Prelude.gcd
, Prelude.lcm
, (Prelude.^)
, (Prelude.^^)
, Prelude.fromIntegral
, Prelude.realToFrac

-- * Other re-exports
, module Data.Bits
, module Data.Int
, module Data.Ratio
, module Data.Word
, module Numeric
, module Numeric.Natural
) where

import Data.Int hiding (Int)
import Data.Word hiding (Word)
import Data.Bits
import Data.Ratio
import Numeric
import Numeric.Natural
