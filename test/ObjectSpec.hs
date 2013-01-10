{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module ObjectSpec where

import Control.Applicative hiding (empty)
import Control.Exception.Base
import Control.Lens
import Data.Dynamic
import Data.Functor.Compose
import GHC.Real
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function as Fun

import Data.Object.Dynamic
import Data.Object.Dynamic.Types
import Data.Object.Dynamic.Presets

type Particle = Object Precise
data Vec a = Vec a a deriving (Eq, Show, Ord, Typeable)

instance (Arbitrary a) => Arbitrary (Vec a) where
  arbitrary = Vec <$> arbitrary <*> arbitrary
  shrink (Vec x y) = Vec <$> shrink x <*> shrink y

data Mass = Mass deriving (Typeable)
instance (Objective o, UseReal o) => Member o Mass where
  type ValType o Mass = UnderlyingReal o

data Velocity = Velocity deriving (Typeable)
instance (Objective o, UseReal o) => Member o Velocity where
  type ValType o Velocity = Vec (UnderlyingReal o)

data Momentum = Momentum deriving (Typeable)
instance (Objective o, UseReal o) => Member o Momentum where
  type ValType o Momentum = Vec (UnderlyingReal o)

data KineticEnergy = KineticEnergy deriving (Typeable)
instance (Objective o, UseReal o) => Member o KineticEnergy where
  type ValType o KineticEnergy = UnderlyingReal o

mass :: MemberLens o Mass
mass = memberLens Mass

velocity :: (UseReal o, Fractional (UnderlyingReal o)) => MemberLens o Velocity
velocity = memberLensDef Velocity $ \this -> do
  m         <- this ^? mass
  Vec mx my <- this ^? momentum
  return $ Vec (mx/m) (my/m)

momentum :: (UseReal o, Fractional (UnderlyingReal o)) => MemberLens o Momentum
momentum = memberLensDef Momentum $ \this -> do
  m         <- this ^? mass
  Vec vx vy <- this ^? velocity
  return $ Vec (m * vx) (m * vy)

kineticEnergy :: (UseReal o, Fractional (UnderlyingReal o)) => MemberLens o KineticEnergy
kineticEnergy = memberLensDef KineticEnergy $ \this -> do
  m         <- this ^? mass
  Vec vx vy <- this ^? velocity
  return $ ((m * vx * vx) + (m * vy * vy)) / 2


fromMassVelocity :: Rational -> Vec Rational -> Particle
fromMassVelocity m v =
  empty & insert Mass m
        & insert Velocity v

fromMassMomentum :: Rational -> Vec Rational -> Particle
fromMassMomentum m v =
  empty & insert Mass m
        & insert Momentum v

instance Eq Particle where
  x == y = x ^? kineticEnergy ==  y ^? kineticEnergy

stackOverflowException :: Selector AsyncException
stackOverflowException StackOverflow = True
stackOverflowException _             = False

spec :: Spec
spec = do
  describe "Point particle behavior as object" $ do
    prop "calculates the energy from mass and velocity" $ \m v ->
      (fromMassVelocity m v) ^? kineticEnergy /= Nothing
    prop "calculates the energy from mass and momentum" $ \m v ->
      (fromMassMomentum m v) ^? kineticEnergy /= Nothing
    it "enters infinite loop when neither velocity nor momentum is known" $
      print ((empty & insert Mass 42 :: Particle) ^? kineticEnergy == Nothing)
       `shouldThrow` stackOverflowException

    prop "reproduces the particle from mass and velocity" $ \m v ->
      (m > 0) ==>
      let p0      :: Particle
          mp1,mp2 :: Maybe Particle
          p0 = fromMassVelocity m v
          mp1 = fromMassMomentum <$> p0^?mass <*> p0^?momentum
          mp2 = (\p1 -> fromMassVelocity <$> p1^?mass <*> p1^?velocity) =<< mp1
      in Just p0 == mp2

  describe "Traversal laws on objects" $ do
    prop "satisfies the first law : t pure ≡ pure" $ \m v ->
      let p = fromMassVelocity m v in
        mass pure p          == (pure p :: Maybe Particle) &&
        velocity pure p      == (pure p :: Either () Particle) &&
        momentum pure p      == (pure p :: [Particle]) &&
        kineticEnergy pure p == (pure p :: ([Particle], Particle))
    prop "satisfies the second law :  fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)" $
      \f' g' m v ->
       let f :: Rational -> Maybe Rational
           g :: Rational -> [Rational]
           f = fmap toRatio . Fun.apply f' . fromRatio
           g = fmap toRatio . Fun.apply g' . fromRatio
           fromRatio :: Rational -> (Integer, Integer)
           fromRatio (x:%y) = (x,y)
           toRatio :: (Integer, Integer) -> Rational
           toRatio (x,y) = x % (if y == 0 then 1 else y)

           p = fromMassVelocity m v
       in
           (fmap (mass f) . (mass g)) p ==
           (getCompose . mass (Compose . fmap f . g) $ p)
