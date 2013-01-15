{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module AcyclicSpec where

import Data.Dynamic
import Data.Object.Dynamic.Type
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck


data A = A deriving Typeable
data B = B deriving Typeable
data C = C deriving Typeable
data D = D deriving Typeable
data E = E deriving Typeable
data F = F deriving Typeable
data G = G deriving Typeable

instance (Objective o) => Member o A where type ValType o A = Integer



spec :: Spec
spec = return ()
