module Data.Object.Dynamic 
(Object, empty) where

import           Control.Lens
import           Data.Object.Dynamic.Types
import qualified Data.Map as Map

-- | An empty object.
empty :: Objective o => o
empty = Table Map.empty ^. from table
