{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Object.Dynamic.Types where

import           Control.Applicative ((<|>))
import           Control.Lens
import           Data.Dynamic
import qualified Data.Map as Map


-- | A basic object type that can contain values of
--   different types.
newtype Object = Object (Map.Map TypeRep Dynamic)
  deriving (Typeable)

instance Show Object where
  show (Object x) = ("Object"++) $ drop 8 $ show x

class Typeable a => KeyType a where
  type ValType a :: *

-- | A Type synonym for a 'Member' 'Lens'.
type Member kt = Lens Object Object (Maybe (ValType kt)) (Maybe (ValType kt))

-- | an empty 'Object' .
--
-- >>> empty
-- Object []

empty :: Object
empty = Object $ Map.empty


-- | Given a key type, create a 'Member' 'Lens' labeled by the key.
--   Here's an example of creating a price tag for objects.
--
-- >>> data Price = Price deriving (Show, Typeable)
-- >>> instance KeyType Price where type ValType Price = Integer
-- >>> let price :: Member Price; price = mkMember Price;
-- >>> let x = set price (Just 120) empty
-- >>> view price empty
-- Nothing
-- >>> view price x
-- Just 120

mkMember :: forall kt. (KeyType kt, Typeable (ValType kt)) => kt -> Member kt
mkMember k1 = lens gettr settr
  where
    gettr :: Object -> Maybe (ValType kt)
    gettr (Object map0) = Map.lookup k map0 >>= fromDynamic
    settr :: Object -> (Maybe (ValType kt)) -> Object
    settr (Object map0) Nothing  = Object $ Map.delete k map0
    settr (Object map0) (Just x) = Object $ Map.insert k (toDyn x) map0
    k :: TypeRep
    k = typeOf k1


-- | Create a 'Member' 'Lens' with a default value.
--   Here's a price tag for objects with default value.
--
-- >>> data Price = Price deriving (Show, Typeable)
-- >>> instance KeyType Price where type ValType Price = Integer
-- >>> let price :: Member Price; price = mkMemberWithDef Price 10;
-- >>> let x = set price (Just 120) empty
-- >>> view price empty
-- Just 10
-- >>> view price x
-- Just 120

mkMemberWithDef ::
  forall kt. (KeyType kt, Typeable (ValType kt)) =>
  kt ->
  ValType kt ->
  Member kt
mkMemberWithDef k1 v1 = lens gettr settr
  where
    gettr :: Object -> Maybe (ValType kt)
    gettr (Object map0) = (Map.lookup k map0 >>= fromDynamic)
                           <|> Just v1
    settr :: Object -> (Maybe (ValType kt)) -> Object
    settr (Object map0) Nothing  = Object $ Map.delete k map0
    settr (Object map0) (Just x) = Object $ Map.insert k (toDyn x) map0
    k :: TypeRep
    k = typeOf k1
