-- | This module provides two presets for the 'Object':
-- 'DIT' for daily use, and 'Precise' for arbitrary precision calculations.

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Object.Dynamic.Presets (DIT, Precise) where

import           Data.Text
import           Data.Object.Dynamic.Underlying


-- $setup
-- >>> import Data.Object.Dynamic.Types
-- >>> let typechecks = True



-- | 'Object' 'DIT' uses 'Double' for real numbers, 'Int' for integers and 'Data.Text.Text' 
-- for strings.
-- 
-- >>> typechecks :: (UnderlyingReal (Object DIT) ~ Double) => Bool
-- True
-- >>> typechecks :: (UnderlyingInteger (Object DIT) ~ Int) => Bool
-- True
-- >>> typechecks :: (UnderlyingString (Object DIT) ~ Data.Text.Text) => Bool
-- True

data DIT
instance UseReal DIT where type UnderlyingReal DIT = Double
instance UseInteger DIT where type UnderlyingInteger DIT = Int
instance UseString DIT where type UnderlyingString DIT = Text

-- | 'Object' 'Precise' uses 'Double' for real numbers, 'Int' for integers and 'Data.Text.Text' 
-- for strings.
-- 
-- >>> typechecks :: (UnderlyingReal (Object Precise) ~ Rational) => Bool
-- True
-- >>> typechecks :: (UnderlyingInteger (Object Precise) ~ Integer) => Bool
-- True
-- >>> typechecks :: (UnderlyingString (Object Precise) ~ Data.Text.Text) => Bool
-- True


data Precise
instance UseReal Precise where type UnderlyingReal Precise = Rational
instance UseInteger Precise where type UnderlyingInteger Precise = Integer
instance UseString Precise where type UnderlyingString Precise = Text
