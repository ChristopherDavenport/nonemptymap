{-# language InstanceSigs #-}
{-# language ScopedTypeVariables #-}

module Data.Map.NonEmpty(
  NonEmptyMap(..) -- Generic Constructor
  -- * Construction
  , singleton -- :: (k, a) -> NonEmptyMap k v
  , fromList -- :: Ord k => [(k, a)] -> Maybe (NonEmptyMap k a)
  -- * Insertion
  , insert -- :: Ord k => k -> a -> NonEmptyMap k a -> NonEmptyMap k a
  , insertWith -- :: Ord k => (a -> a -> a) -> k -> a -> NonEmptyMap k a -> NonEmptyMap k a
  , insertWithKey -- :: Ord k => (k -> a -> a -> a) -> k -> a -> NonEmptyMap k a -> NonEmptyMap k a
  , insertLookupWithKey -- :: Ord k => (k -> a -> a -> a) -> k -> a -> NonEmptyMap k a -> (Maybe a, NonEmptyMap k a)
  -- * Deletion/Update
  , delete -- :: Ord k => k -> NonEmptyMap k a -> Map.Map k a
  , adjust -- :: Ord k => (a -> a) -> k -> NonEmptyMap k a -> NonEmptyMap k a 
  , update -- :: Ord k => (a -> Maybe a) -> k -> NonEmptyMap k a -> Map.Map k a
  , alter  -- :: Ord k => (Maybe a -> Maybe a) -> k -> NonEmptyMap k a -> Map.Map k a
  , alterF -- :: forall f k a. (Functor f, Ord k) => (Maybe a -> f (Maybe a)) -> k -> NonEmptyMap k a -> f (Map.Map k a)
  -- * Query
  , lookup -- :: Ord k => k -> NonEmptyMap k a -> Maybe a
  , (!?)   -- :: Ord k => NonEmptyMap k a -> k -> Maybe a
  , findWithDefault -- :: Ord k => a -> k -> NonEmptyMap k a -> a
  , member -- :: Ord k => k -> NonEmptyMap k a -> Bool
  , notMember -- :: Ord k => k -> NonEmptyMap k a -> Bool
  -- * Size 
  , size -- :: NonEmptyMap k a -> In
  -- * Conversions
  -- * Lists
  , toList -- :: NonEmptyMap k a -> [(k, a)]
) where

import qualified Data.Map                   as Map
import Data.Maybe                           (fromMaybe, isJust)
import Data.Functor.Classes                 (Eq1, Eq2, liftEq2, liftEq
                                            , Ord1, Ord2, liftCompare2, liftCompare
                                            , Show1, Show2, liftShowsPrec2, showsUnaryWith, liftShowsPrec, liftShowList2
                                            , Read1, liftReadsPrec, readsData, readsUnaryWith, liftReadList)
import Prelude                              hiding (lookup)


-- A NonEmptyMap of keys k to values a
data NonEmptyMap k a = NonEmptyMap (k, a) (Map.Map k a)

-- Instances

instance Eq2 NonEmptyMap where
  liftEq2 :: (k -> l -> Bool) -> (m -> n -> Bool) -> NonEmptyMap k m -> NonEmptyMap l n -> Bool
  liftEq2 eqk eqa nem nen =
    size nen == size nen && liftEq (liftEq2 eqk eqa) (toList nem) (toList nen)

instance Eq k => Eq1 (NonEmptyMap k) where
  liftEq = liftEq2 (==)

instance Ord2 NonEmptyMap where
  liftCompare2 cmpk cmpv m n =
    liftCompare (liftCompare2 cmpk cmpv) (toList m) (toList n)

instance Ord k => Ord1 (NonEmptyMap k) where
  liftCompare = liftCompare2 compare

instance Show2 NonEmptyMap where
  liftShowsPrec2 spk slk spv slv d m =
    showsUnaryWith (liftShowsPrec sp sl) "fromList" d (toList m)
    where
      sp = liftShowsPrec2 spk slk spv slv
      sl = liftShowList2 spk slk spv slv

instance Show k => Show1 (NonEmptyMap k) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

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
insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> NonEmptyMap k a -> NonEmptyMap k a
insertWithKey f key value (NonEmptyMap (k, a) m) = 
  if k == key then NonEmptyMap (key, f key value a) m
  else NonEmptyMap (k, a) (Map.insertWithKey f key value m)

--  insertLookupWithKey
insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> NonEmptyMap k a -> (Maybe a, NonEmptyMap k a)
insertLookupWithKey f key value (NonEmptyMap (k, a) m) = 
  if k == key then (Just a, NonEmptyMap(key, f key value a) m)
  else fmap (NonEmptyMap (k, a)) (Map.insertLookupWithKey f key value m)

-- Deletion/Update
delete :: Ord k => k -> NonEmptyMap k a -> Map.Map k a
delete key (NonEmptyMap (k, a) m) | key == k  = m
delete key (NonEmptyMap (k, a) m)             = Map.insert k a (Map.delete k m)

adjust :: Ord k => (a -> a) -> k -> NonEmptyMap k a -> NonEmptyMap k a 
adjust f key (NonEmptyMap (k, a) m) | key == k  = NonEmptyMap (key, f a) m
adjust f key (NonEmptyMap (k, a) m)             = NonEmptyMap (k, a) (Map.adjust f key m)

update :: Ord k => (a -> Maybe a) -> k -> NonEmptyMap k a -> Map.Map k a
update f key (NonEmptyMap (k, a) m) | key == k = case f a of
  Just a -> Map.insert k a m
  Nothing -> m
update f key (NonEmptyMap (k, a) m)           = Map.insert k a (Map.update f key m)

alter :: Ord k => (Maybe a -> Maybe a) -> k -> NonEmptyMap k a -> Map.Map k a
alter f key (NonEmptyMap (k, a) m) | key == k = case f (Just a) of
  Just a -> Map.insert k a m
  Nothing -> m
alter f key (NonEmptyMap (k, a) m)            = Map.insert k a (Map.alter f key m)

alterF :: forall f k a. (Functor f, Ord k) => (Maybe a -> f (Maybe a)) -> k -> NonEmptyMap k a -> f (Map.Map k a) 
alterF f key (NonEmptyMap (k, a) m) | key == k = insideF <$> f (Just a)
  where
    insideF :: Maybe a -> Map.Map k a
    insideF (Just a)  = Map.insert k a m
    insideF Nothing   = m
alterF f key (NonEmptyMap (k, a) m)            = Map.insert k a <$> Map.alterF f key m

-- Query

lookup :: Ord k => k -> NonEmptyMap k a -> Maybe a
lookup key (NonEmptyMap (k, a) m) | key == k = Just a
lookup key (NonEmptyMap _ m)                 = Map.lookup key m

(!?) :: Ord k => NonEmptyMap k a -> k -> Maybe a
(!?) nem k = lookup k nem

findWithDefault :: Ord k => a -> k -> NonEmptyMap k a -> a
findWithDefault a key nem = fromMaybe a (lookup key nem)

member :: Ord k => k -> NonEmptyMap k a -> Bool
member key nem = isJust (lookup key nem)

notMember :: Ord k => k -> NonEmptyMap k a -> Bool
notMember k nem = not $ member k nem

-- Size 
size :: NonEmptyMap k a -> Int
size (NonEmptyMap _ m) = 1 + Map.size m   

-- Conversions


-- Lists
toList :: NonEmptyMap k a -> [(k, a)]
toList (NonEmptyMap tup m) = tup : Map.toList m