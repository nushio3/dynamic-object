{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module AcyclicSpec where

import Control.Lens
import Data.Dynamic
import Data.Object.Dynamic
import Data.Object.Dynamic.Presets
import Data.Object.Dynamic.Type
import Test.Hspec
import Test.Hspec.QuickCheck (prop)


data A = A deriving Typeable
data B = B deriving Typeable
data C = C deriving Typeable
data D = D deriving Typeable
data E = E deriving Typeable


instance (Objective o) => Member o A where
  type ValType o A = Integer
  memberLookup = acyclically $ do
    e <- its E
    return $ 1+e

instance (Objective o) => Member o B where
  type ValType o B = Integer
  memberLookup = acyclically $ do
    a <- its A
    return $ 1+a
instance (Objective o) => Member o C where
  type ValType o C = Integer
  memberLookup = acyclically $ do
    a <- its A
    b <- its B
    return $ 1+a+b
instance (Objective o) => Member o D where
  type ValType o D = Integer
  memberLookup = acyclically $ do
    a <- its A
    b <- its B
    c <- its C
    a' <- its A
    b' <- its B
    return $ 1+a+b+c+a'+b'
instance (Objective o) => Member o E where
  type ValType o E = Integer
  memberLookup = acyclically $ do
    a <- its A
    b <- its B
    c <- its C
    d <- its D
    return $ 1+a+b+c+d

obj0 :: Object DIT
obj0 = empty

spec :: Spec
spec = describe "Complicated Object" $ do
  it "detects loop if it is empty." $ do
    obj0 ^? memberLens E == Nothing
  it "fallbacks correctly even if it is complicated." $ do
    insert A 1 obj0 ^? memberLens E `shouldBe` Just 19
  prop "does not fail for any possible combination of members" $
    \sw0 ->
      let sw1 :: [Bool]
          sw1 = sw0 ++ repeat False
          sw i f = if sw1!!i then f else id
          obj1 = obj0
               & sw 0 (insert A 1)
               & sw 1 (insert B 2)
               & sw 2 (insert C 3)
               & sw 3 (insert D 4)
               & sw 4 (insert E 5)
      in "***" /= (drop 3 $  show (obj1 ^? memberLens A))