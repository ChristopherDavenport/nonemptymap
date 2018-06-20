{-# language InstanceSigs #-}
module Data.Map.NonEmpty(
  NonEmptyMap(..), -- Generic Constructor
  singleton -- :: (k, v) -> NonEmptyMap k v
) where

import qualified Data.Map                   as Map


-- A NonEmptyMap of keys k to values v
data NonEmptyMap k v = NonEmptyMap (k, v) (Map.Map k v)


instance Functor (NonEmptyMap k) where
  fmap :: (a -> b) -> NonEmptyMap k a -> NonEmptyMap k b
  fmap f (NonEmptyMap (k, v) map) =  NonEmptyMap (k, f v) (fmap f map)

-- Construction
singleton :: (k, v) -> NonEmptyMap k v
singleton tup = NonEmptyMap tup Map.empty




-- , insert

-- , insertWith
-- , insertWithKey
-- , insertLookupWithKey
