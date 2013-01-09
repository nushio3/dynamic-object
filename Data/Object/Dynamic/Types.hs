{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Object.Dynamic.Types where

import           Control.Applicative ((<$>),pure)
import qualified Control.Category as Cat ((.))
import           Control.Lens
import           Control.Lens.Iso
import           Data.Dynamic
import           Data.HList.FakePrelude
import           Data.HList.HListPrelude
import           Data.HList.Record
import           Data.Ratio
import qualified Data.Map as Map

-- | The 'Object' type, where @u@ carrying the underlying types information.
newtype Object u = Object {unObject :: Table}
instance Objective (Object u) where
  table = iso unObject Object

-- | The 'Table' within an 'Object' that carries all the member data.
newtype Table = Table {unTable :: Map.Map TypeRep Dynamic}

-- | @o@ is an 'Objective' if it's equivalent to the 'Table'
--   given its type information.
class Objective o where
  table :: Simple Iso o Table
  tableMap :: Simple Iso o (Map.Map TypeRep Dynamic)
  tableMap = table Cat.. (iso unTable Table)

-- |
class (Objective o) => Member o memb where
  type ValType o memb :: *

type MemberLens memb =
  (Member o memb, Typeable (ValType o memb))
        => Simple Traversal o (ValType o memb)


memberLens :: (Objective o, Member o memb,
             Typeable memb, Typeable (ValType o memb))
     => memb -> Simple Traversal o (ValType o memb)
memberLens memb0 r2ar obj = case Map.lookup tag (unTable tbl) of
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

-- | Given a pair of member tag and an

insert :: (Objective o, Member o memb, ValType o memb ~ val,
           Typeable memb, Typeable val)
  => memb -> val -> o -> o
insert memb0 val0 = over tableMap $ Map.insert tag (toDyn val0)
  where
    tag :: TypeRep
    tag = typeOf memb0
