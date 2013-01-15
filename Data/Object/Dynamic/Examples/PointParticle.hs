-- | Here we use @dynamic-object@ to descibe the concept of point-like particles from
-- classical mechanics. Also read the HSpec tests :
-- <https://github.com/nushio3/dynamic-object/blob/master/test/ObjectSpec.hs>
-- for more details.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Object.Dynamic.Examples.PointParticle
       (Vec(..),
        Mass(..), Velocity(..), Momentum(..), KineticEnergy(..),
        mass, velocity, momentum, kineticEnergy,
        fromMassVelocity, fromMassMomentum,
        laserBeam,
        duck, lens, banana, envelope, ghost
        )
       where

import Control.Applicative hiding (empty)
import Control.Lens hiding (lens)
import Data.Dynamic
import Data.String
import Test.QuickCheck

import Data.Object.Dynamic
import Data.Object.Dynamic.Type

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XScopedTypeVariables
-- >>> import Data.Object.Dynamic.Presets
-- >>> import Data.Maybe

-- | First, let us create a tiny two-dimensional vector class.
-- We make it an instance of 'Arbitrary' to use them later for tests.
data Vec a = Vec a a deriving (Eq, Show, Ord, Typeable)


instance (Arbitrary a) => Arbitrary (Vec a) where
  arbitrary = Vec <$> arbitrary <*> arbitrary
  shrink (Vec x y) = Vec <$> shrink x <*> shrink y

-- | Now, let us introduce the concepts of 'Mass', 'Velocity',
-- 'Momentum' and 'KineticEnergy'. Any such concepts are described
-- in terms of 'Member' labels.
data Mass = Mass deriving (Typeable)
instance (Objective o, UseReal o) => Member o Mass where
  type ValType o Mass = UnderlyingReal o

-- | To define a member with compound types like vector of real numbers,
-- we use 'UnderlyingReal' to
-- ask the object which real value it prefers, then put the response
-- into the type constructors.
--
-- We also give a fallback accessor here. If the 'velocity' field is missing, we attempt to re-calculate it
-- from the 'mass' and 'momentum'. Here is how we can do that.

data Velocity = Velocity deriving (Typeable)
instance (Objective o, UseReal o,  Fractional (UnderlyingReal o)) => Member o Velocity where
  type ValType o Velocity = Vec (UnderlyingReal o)
  memberLookup = acyclically $ do  
    m         <- its Mass
    Vec mx my <- its Momentum
    return $ Vec (mx/m) (my/m)
                   
-- | If the 'momentum' field is missing, we re-calculate it
-- from the 'mass' and 'velocity'.
data Momentum = Momentum deriving (Typeable)
instance (Objective o, UseReal o,  Fractional (UnderlyingReal o)) => Member o Momentum where
  type ValType o Momentum = Vec (UnderlyingReal o)
  memberLookup = acyclically $ do
    m         <- its Mass
    Vec vx vy <- its Velocity
    return $ Vec (m * vx) (m * vy)
  
-- | 'kineticEnergy', unless given explicitly, is defined in terms of 'mass' and 'velocity' .
data KineticEnergy = KineticEnergy deriving (Typeable)
instance (Objective o, UseReal o,  Fractional (UnderlyingReal o)) => Member o KineticEnergy where
  type ValType o KineticEnergy = UnderlyingReal o
  memberLookup = acyclically $ do
    m         <- its Mass
    Vec vx vy <- its Velocity
    return $ ((m * vx * vx) + (m * vy * vy)) / 2

-- | Now we define the lenses.
mass :: MemberLens o Mass
mass = memberLens Mass

velocity :: MemberLens o Velocity
velocity = memberLens Velocity

momentum :: (UseReal o, Fractional (UnderlyingReal o)) => MemberLens o Momentum
momentum = memberLens Momentum

kineticEnergy :: (UseReal o, Fractional (UnderlyingReal o)) => MemberLens o KineticEnergy
kineticEnergy = memberLens KineticEnergy 

-- | We can write functions that would construct a point particle from
--   its mass and velocity. And we can make the function polymorphic over the
--   representation of the real numbers the objects prefer.
fromMassVelocity :: (Objective o, UseReal o, Fractional real, real ~ (UnderlyingReal o))
                    => real -> Vec real -> o
fromMassVelocity m v =
  empty & insert Mass m
        & insert Velocity v

-- | We can also construct a point particle from
--   its mass and momentum.
fromMassMomentum :: (Objective o, UseReal o, Fractional real, real ~ (UnderlyingReal o))
                    => real -> Vec real -> o
fromMassMomentum m v =
   empty & insert Mass m
         & insert Momentum v

-- | We define an instance of point-like particle.  And again, we can
-- keep it polymorphic, so that anyone can choose its concrete type
-- later, according to their purpose.  Thus we will achieve the
-- polymorphic encoding of the knowledge of this world, in Haskell.
--
-- >>> (laserBeam :: Object DIT) ^? kineticEnergy
-- Just 1631.25
-- >>> (laserBeam :: Object Precise) ^? kineticEnergy
-- Just (6525 % 4)
--
-- Moreover, we can ask Ichiro to sign the ball. Usually, we needed to
-- create a new data-type to add a new field. But with
-- 'dynamic-object' we can do so without changing the type of the
-- ball. So, we can put our precious, one-of-a-kind ball
-- into toybox together with less uncommon balls, and with various
-- other toys. And still, we can safely access the contents of the
-- toybox without runtime errors, and e.g. see which toy is the heaviest.
--
-- >>> let (mySpecialBall :: Object DIT) = laserBeam & insert Autograph "Ichiro Suzuki"
-- >>> let toybox = [laserBeam, mySpecialBall]
-- >>> let toybox2 = toybox ++ [duck, lens, banana, envelope, ghost]
-- >>> maximum $ mapMaybe (^?mass) toybox2
-- 5.2

laserBeam :: (Objective o, UseReal o, Fractional real, real ~ (UnderlyingReal o)) => o
laserBeam = fromMassVelocity 0.145 (Vec 150 0) -- a baseball thrown by
            -- Ichiro

duck, lens, banana :: (Objective o, UseReal o, Fractional real, real ~ (UnderlyingReal o)) => o
duck = empty & insert Mass 5.2
lens = empty & insert Mass 0.56
banana = empty & insert Mass 0.187

envelope :: (Objective o, UseReal o, UseString o, Fractional (UnderlyingReal o),
             IsString (UnderlyingString o)) => o
envelope = empty & insert Mass 0.025
                 & insert Autograph (fromString "Edward Kmett")

ghost :: (Objective o) => o
ghost = empty

data Autograph = Autograph deriving Typeable
instance (Objective o, UseString o) => Member o Autograph where
  type ValType o Autograph = UnderlyingString o
