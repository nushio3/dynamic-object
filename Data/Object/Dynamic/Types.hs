{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module Data.Object.Dynamic.Types where

import           Control.Applicative ((<$>),pure, (<|>))
import qualified Control.Category as Cat ((.))
import           Control.Lens
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

-- | This means that @memb@ is one of the  member labels
-- of @o@. The 'ValType' of the member depends both on the label
-- and (the underlying types of) the object.
class (Objective o,Typeable memb, Typeable (ValType o memb)) => Member o memb where
  type ValType o memb :: *

-- | The lens for accessing the 'Member' of the 'Object'.
type MemberLens o memb = (Member o memb) => Simple Traversal o (ValType o memb)

-- | A utility function for creating a 'MemberLens' .
mkMemberLensDef ::
  (Member o memb)
  => memb                          -- ^ member label
  -> (o -> Maybe (ValType o memb)) -- ^ default value, in case
                                   -- the member is not in the map
  -> MemberLens o memb             -- ^ generated lens

mkMemberLensDef label0 def0 r2ar obj =
  case (Map.lookup key (unTable tbl) >>= fromDynamic) <|> def0 obj of
    Just r -> go r
    Nothing -> pure obj
  where
    tbl :: Table
    tbl = obj ^. table
    key :: TypeRep
    key = typeOf label0

    go r = (\r' -> obj & over tableMap (Map.insert key (toDyn r')) )
           <$> r2ar r

-- | create a 'MemberLens' without any default values.

mkMemberLens :: (Member o memb) => memb -> MemberLens o memb
mkMemberLens label0 = mkMemberLensDef label0 (const Nothing)


-- | Given a pair of 'Member' label and a value, create the data field
--  for the member and inserts the value.

insert :: (Objective o, Member o memb, ValType o memb ~ val,
           Typeable memb, Typeable val)
  => memb -> val -> o -> o
insert label0 val0 = over tableMap $ Map.insert tag (toDyn val0)
  where
    tag :: TypeRep
    tag = typeOf label0
