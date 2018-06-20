{-# language InstanceSigs #-}
module Data.Map.NonEmpty(
  NonEmptyMap(..) -- Generic Constructor
  -- * Construction
  , singleton -- :: (k, a) -> NonEmptyMap k v
  , fromList -- :: Ord k => [(k, a)] -> Maybe (NonEmptyMap k a)
  -- * Insertion
  , insert -- :: Ord k => k -> a -> NonEmptyMap k a -> NonEmptyMap k a
  , insertWith -- :: Ord k => (a -> a -> a) -> k -> a -> NonEmptyMap k a -> NonEmptyMap k a
  -- * Deletion/Update
  , delete -- :: Ord k => k -> NonEmptyMap k a -> Map.Map k a
) where

import qualified Data.Map                   as Map

-- A NonEmptyMap of keys k to values a
data NonEmptyMap k a = NonEmptyMap (k, a) (Map.Map k a)

-- Instances

instance Functor (NonEmptyMap k) where
  fmap :: (a -> b) -> NonEmptyMap k a -> NonEmptyMap k b
  fmap f (NonEmptyMap (k, v) map) =  NonEmptyMap (k, f v) (fmap f map)

-- Construction
singleton :: (k, a) -> NonEmptyMap k a
singleton tup = NonEmptyMap tup Map.empty

fromList :: Ord k => [(k, a)] -> Maybe (NonEmptyMap k a)
fromList []       = Nothing
fromList (x : xa) = Just $ NonEmptyMap x (Map.fromList xa)

-- Insertion

-- , insert
insert :: Ord k => k -> a -> NonEmptyMap k a -> NonEmptyMap k a
insert = insertWith const

-- , insertWith
insertWith :: Ord k => (a -> a -> a) -> k -> a -> NonEmptyMap k a -> NonEmptyMap k a
insertWith f key value (NonEmptyMap (k, a) m) | key == k  = NonEmptyMap (key, f value a) m
insertWith f key value (NonEmptyMap (k, a) m)             = NonEmptyMap (k, a) (Map.insertWith f key value m)
-- , insertWithKey
-- , insertLookupWithKey

-- Deletion/Update

delete :: Ord k => k -> NonEmptyMap k a -> Map.Map k a
delete key (NonEmptyMap (k, a) m) | key == k  = m
delete key (NonEmptyMap (k, a) m)             = Map.insert k a (Map.delete k m)