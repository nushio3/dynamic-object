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
import qualified Control.Monad.RWS as RWS
import           Data.Dynamic
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | The 'Object' type, where @u@ carrying the information of its underlying types.
newtype Object u = Object {unObject :: Table}
instance Objective (Object u) where
  table = iso unObject Object



-- | The 'Table' within an 'Object' that carries all the member data.
newtype Table = Table {unTable :: TableMap}

-- | The 'Map.Map' type within the table.
type TableMap = Map.Map TypeRep Dynamic

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
  memberLens :: memb -> MemberLens o memb
  memberLens = mkMemberLens
  memberLookup :: memb -> Lookup o (ValType o memb)
  memberLookup = mkMemberLookup

-- | The lens for accessing the 'Member' of the 'Object'.
type MemberLens o memb = (Member o memb) => Simple Traversal o (ValType o memb)


-- | A utility function for defining a 'MemberLens' .
mkMemberLens ::
  (Member o memb)
  => memb                    -- ^ member label
  -> MemberLens o memb       -- ^ generated lens

mkMemberLens label0 r2ar obj =
  case fmap fst $ RWS.evalRWST (memberLookup label0) obj Set.empty of
    Just r -> go r
    Nothing -> pure obj
  where
    tbl :: Table
    tbl = obj ^. table
    key :: TypeRep
    key = typeOf label0

    go r = (\r' -> obj & over tableMap (Map.insert key (toDyn r')) )
           <$> r2ar r



-- | A utility function for defining a 'MemberLookup', with a default computation
-- for the case the member is missing.
mkMemberLookupDef ::
  (Member o memb)
  => memb                    -- ^ member label
  -> Lookup o (ValType o memb) -- ^ default accessor when the record is missing
  -> Lookup o (ValType o memb) -- ^ member accessor

mkMemberLookupDef label0 def0 = do
  obj <- RWS.ask
  let
    tblMap :: TableMap
    tblMap = obj ^. tableMap
    key :: TypeRep
    key = typeOf label0

  case (Map.lookup key tblMap >>= fromDynamic) of
    Just ret -> RWS.lift $ return ret
    Nothing  -> do
      usedKeys <- RWS.get
      case key `Set.member` usedKeys of
        -- loop detected, further search truncated.
        True -> RWS.lift $ Nothing
        -- invoke the default computation.
        False -> do
          RWS.put (key `Set.insert` usedKeys)
          ret <- def0
          RWS.modify (Set.delete key)
          return ret

-- | Defining a 'MemberLookup', without default.
mkMemberLookup ::
  (Member o memb)
  => memb                    -- ^ member label
  -> Lookup o (ValType o memb) -- ^ member accessor
mkMemberLookup label0 = mkMemberLookupDef label0 (RWS.lift Nothing)


-- | Given a pair of 'Member' label and a value, create the data field
--  for the member and inserts the value.

insert :: (Objective o, Member o memb, ValType o memb ~ val,
           Typeable memb, Typeable val)
  => memb -> val -> o -> o
insert label0 val0 = over tableMap $ Map.insert tag (toDyn val0)
  where
    tag :: TypeRep
    tag = typeOf label0


-- | Lookup monad is used to lookup a member of the object
--   with infinite-loop detection.
type Lookup o a = RWS.RWST o () (Set.Set TypeRep) Maybe a

this :: Member o memb => memb -> Lookup o (ValType o memb)
this = memberLookup