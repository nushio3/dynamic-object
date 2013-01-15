-- | This module re-exports things needed to use 'Object's, 
-- and adds a few utility functions.

module Data.Object.Dynamic 
       (Object, empty, insert, MemberLens,
        module Data.Object.Dynamic.Underlying
       ) where

import           Control.Lens
import           Data.Object.Dynamic.Type
import           Data.Object.Dynamic.Underlying
import qualified Data.Map as Map

-- | An empty object.
empty :: Objective o => o
empty = Table Map.empty ^. from table
