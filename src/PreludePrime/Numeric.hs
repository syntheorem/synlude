{-# LANGUAGE ExplicitNamespaces, FlexibleContexts, TypeFamilies, UndecidableSuperClasses #-}

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

-- * Additional declarations
, SignedIntegral(type ToUnsigned)
, UnsignedIntegral(type ToSigned)

-- * Other re-exports
, module Data.Int
, module Data.Word
, module Data.Bits
, module Data.Ratio
, module Numeric
, module Numeric.Natural
) where

import Data.Int hiding (Int)
import Data.Word hiding (Word)
import Data.Bits
import Data.Ratio
import Numeric
import Numeric.Natural
import Prelude

-- | Class of signed integral values.
class (Integral a, UnsignedIntegral (ToUnsigned a)) => SignedIntegral a where
  -- | Unsigned integral type of the same size.
  type ToUnsigned a

-- | Class of unsigned integral values.
class (Integral a, SignedIntegral (ToSigned a)) => UnsignedIntegral a where
  -- | Signed integral type of the same size.
  type ToSigned a

instance SignedIntegral   Integer where type ToUnsigned Integer = Natural
instance SignedIntegral   Int     where type ToUnsigned Int     = Word
instance SignedIntegral   Int8    where type ToUnsigned Int8    = Word8
instance SignedIntegral   Int16   where type ToUnsigned Int16   = Word16
instance SignedIntegral   Int32   where type ToUnsigned Int32   = Word32
instance SignedIntegral   Int64   where type ToUnsigned Int64   = Word64
instance UnsignedIntegral Natural where type ToSigned   Natural = Integer
instance UnsignedIntegral Word    where type ToSigned   Word    = Int
instance UnsignedIntegral Word8   where type ToSigned   Word8   = Int8
instance UnsignedIntegral Word16  where type ToSigned   Word16  = Int16
instance UnsignedIntegral Word32  where type ToSigned   Word32  = Int32
instance UnsignedIntegral Word64  where type ToSigned   Word64  = Int64
