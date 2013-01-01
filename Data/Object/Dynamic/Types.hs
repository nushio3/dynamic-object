{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Object.Dynamic.Types where

import qualified Data.Map as Map
import           Data.Dynamic
import           Control.Lens

-- | A basic object type that can contain values of 
--   different types.
newtype Object = Object (Map.Map TypeRep Dynamic)
  deriving (Typeable)

instance Show Object where
  show (Object x) = ("Object"++) $ drop 8 $ show x

class Typeable a => KeyType a where
  type ValType a :: *

-- | A Type synonym for a Method lens.
type Method kt = Lens Object Object (Maybe (ValType kt)) (Maybe (ValType kt))

-- | an empty @Object@ .
--
-- >>> empty
-- Object []


empty :: Object
empty = Object $ Map.empty

mkMethod :: forall kt. (KeyType kt, Typeable (ValType kt)) => kt -> Method kt
mkMethod k1 = lens gettr settr
  where
    gettr :: Object -> Maybe (ValType kt)
    gettr (Object map0) = Map.lookup k map0 >>= fromDynamic
    settr :: Object -> (Maybe (ValType kt)) -> Object
    settr (Object map0) Nothing  = Object $ Map.delete k map0
    settr (Object map0) (Just x) = Object $ Map.insert k (toDyn x) map0
    k :: TypeRep
    k = typeOf k1
