module ObjectSpec where


import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Data.Object.Dynamic
import Data.Object.Dynamic.Presets


newtype MassVelocity = MassVelocity { fromMassVelocity :: Object Precise }
newtype MassMomentum = MassMomentum { fromMassMomentum :: Object Precise }



spec :: Spec
spec = return ()