{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Object.Dynamic.Presets where

import           Data.Text
import           Data.Ratio

import           Data.Object.Dynamic.Underlying


data DIT
instance UseReal DIT where type UnderlyingReal DIT = Double
instance UseInteger DIT where type UnderlyingInteger DIT = Int
instance UseString DIT where type UnderlyingString DIT = Text


data Precise
instance UseReal Precise where type UnderlyingReal Precise = Rational
instance UseInteger Precise where type UnderlyingInteger Precise = Integer
instance UseString Precise where type UnderlyingString Precise = Text
