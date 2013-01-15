{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Object.Dynamic.Underlying where

import           Data.Dynamic
import           Data.Object.Dynamic.Type

-- | The declaration of @instance@ 'Objective' @obj@ , @instance@ 'UseReal' @obj@ means
-- that the @obj@ is an 'Object' and is ready to tell the 'Member's which type to use 
-- to represent the real numbers.
class (Typeable (UnderlyingReal a)) => UseReal a  where
  type UnderlyingReal a :: *
instance  UseReal u => UseReal (Object u) where
  type UnderlyingReal (Object u) = UnderlyingReal u

-- | Underlying integer types.
class (Typeable (UnderlyingInteger a)) => UseInteger a  where
  type UnderlyingInteger a :: *
instance  UseInteger u => UseInteger (Object u) where
  type UnderlyingInteger (Object u) = UnderlyingInteger u

-- | Underlying string types.
class (Typeable (UnderlyingString a)) => UseString a  where
  type UnderlyingString a :: *
instance  UseString u => UseString (Object u) where
  type UnderlyingString (Object u) = UnderlyingString u


