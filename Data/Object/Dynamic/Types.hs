{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module Data.Object.Dynamic.Types where

import           Control.Applicative ((<$>),pure)
import qualified Control.Category as Cat ((.))
import           Control.Lens
import           Control.Lens.Iso
import           Data.Dynamic
import qualified Data.Map as Map

-- | The 'Object' type, where @u@ carrying the information of its underlying types.
newtype Object u = Object {unObject :: Table}
instance Objective (Object u) where
  table = iso unObject Object

-- | The 'Table' within an 'Object' that carries all the member data.
newtype Table = Table {unTable :: Map.Map TypeRep Dynamic}

-- | @o@ is an 'Objective' if given its type information,
-- there is an equivalence between @o@ and the 'Table'.
class Objective o where
  table :: Simple Iso o Table
  tableMap :: Simple Iso o (Map.Map TypeRep Dynamic)
  tableMap = table Cat.. (iso unTable Table)

-- | An instance of this type class declares that @memb@ is a member label
-- of @o@. The 'ValType' of the member depends both on the label
-- and (the underlying types of) the object.
class (Objective o) => Member o memb where
  type ValType o memb :: *

-- | The lens for accessing the 'Member' of the 'Object'.
type MemberLens memb =
  (Member o memb, Typeable (ValType o memb))
        => Simple Traversal o (ValType o memb)


-- | A utility function for creating a 'MemberLens'
memberLensDef ::
  (Objective o, Member o memb, Typeable memb, Typeable (ValType o memb))
  => memb                               -- ^ member label
  -> (o -> Maybe (ValType o memb))      -- ^ default result retrieval
  -> Simple Traversal o (ValType o memb)-- ^ generated lens

memberLensDef memb0 def0 r2ar obj =
  case Map.lookup tag (unTable tbl) of
    Just dr -> case fromDynamic dr of
      Just r -> (\r' -> obj & over tableMap
            (Map.insert tag (toDyn r')) ) <$> r2ar r
      Nothing -> pure obj
    Nothing -> pure obj
  where
    tbl :: Table
    tbl = obj ^. table
    tag :: TypeRep
    tag = typeOf memb0

-- | Given a pair of 'Member' tag and a value, create the data field
--  for the member and inserts the value.

insert :: (Objective o, Member o memb, ValType o memb ~ val,
           Typeable memb, Typeable val)
  => memb -> val -> o -> o
insert memb0 val0 = over tableMap $ Map.insert tag (toDyn val0)
  where
    tag :: TypeRep
    tag = typeOf memb0
