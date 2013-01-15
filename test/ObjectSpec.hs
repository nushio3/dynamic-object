{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module ObjectSpec where

import Control.Applicative hiding (empty)
import Control.Exception.Base
import Control.Lens
import Data.Dynamic
import Data.Functor.Compose
import GHC.Real (Ratio((:%)), (%))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function as Fun

import Data.Object.Dynamic
import Data.Object.Dynamic.Examples.PointParticle
import Data.Object.Dynamic.Presets
import Data.Object.Dynamic.Type



newtype Particle = Particle (Object Precise)
  deriving (Typeable, Objective)
instance UseReal Particle where
  type UnderlyingReal Particle = Rational


instance Eq Particle where
  x == y = x ^? kineticEnergy ==  y ^? kineticEnergy

stackOverflowException :: Selector AsyncException
stackOverflowException StackOverflow = True
stackOverflowException _             = False

spec :: Spec
spec = do
  describe "Point particle library" $ do
    prop "calculates the energy from mass and velocity" $ \m v ->
      (fromMassVelocity m v :: Particle) ^? kineticEnergy /= Nothing
    prop "calculates the energy from mass and momentum" $ \m v ->
      (fromMassMomentum m v :: Particle) ^? kineticEnergy /= Nothing
    it "avoids infinite loop even if neither velocity nor momentum is known" $
      (empty & insert Mass 42 :: Particle) ^? kineticEnergy == Nothing


    prop "reproduces the particle from mass and velocity" $ \m v ->
      (m > 0) ==>
      let p0      :: Particle
          mp1,mp2 :: Maybe Particle
          p0 = fromMassVelocity m v
          mp1 = fromMassMomentum <$> p0^?mass <*> p0^?momentum
          mp2 = (\p1 -> fromMassVelocity <$> p1^?mass <*> p1^?velocity) =<< mp1
      in Just p0 == mp2

  describe "Objects, as Traversal," $ do
    prop "satisfies the first law : t pure ≡ pure" $ \m v ->
      let p = fromMassVelocity m v in
        mass pure p          == (pure p :: Maybe Particle) &&
        velocity pure p      == (pure p :: Either () Particle) &&
        momentum pure p      == (pure p :: [Particle]) &&
        kineticEnergy pure p == (pure p :: ([Particle], Particle))
    prop "satisfies the second law : fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)" $
      \f' g' m v ->
       let f :: Rational -> Maybe Rational
           g :: Rational -> [Rational]
           f = fmap toRatio . Fun.apply f' . fromRatio
           g = fmap toRatio . Fun.apply g' . fromRatio
           fromRatio :: Rational -> (Integer, Integer)
           fromRatio (x:%y) = (x,y)
           toRatio :: (Integer, Integer) -> Rational
           toRatio (x,y) = x % (if y == 0 then 1 else y)

           p :: Particle
           p = fromMassVelocity m v
       in
           (fmap (mass f) . (mass g)) p ==
           (getCompose . mass (Compose . fmap f . g) $ p) &&
           (fmap (kineticEnergy f) . (kineticEnergy g)) p ==
           (getCompose . kineticEnergy (Compose . fmap f . g) $ p)
    prop "satisfies the second law for vector members, too" $
      \f' g' m v ->
       let f :: Vec Rational -> Maybe (Vec Rational)
           g :: Vec Rational -> [Vec Rational]
           f = fmap toRatio . Fun.apply f' . fromRatio
           g = fmap toRatio . Fun.apply g' . fromRatio
           fromRatio :: Vec Rational -> ((Integer, Integer), (Integer, Integer))
           fromRatio (Vec (ax:%ay) (bx:%by)) = ((ax,ay), (bx,by))
           toRatio :: ((Integer, Integer), (Integer, Integer)) -> Vec Rational
           toRatio ((ax,ay), (bx,by)) = Vec (mk ax ay) (mk bx by)
           mk x y = x % (if y == 0 then 1 else y)

           p :: Particle
           p = fromMassVelocity m v
       in
           (fmap (velocity f) . (velocity g)) p ==
           (getCompose . velocity (Compose . fmap f . g) $ p) &&
           (fmap (momentum f) . (momentum g)) p ==
           (getCompose . momentum (Compose . fmap f . g) $ p)
