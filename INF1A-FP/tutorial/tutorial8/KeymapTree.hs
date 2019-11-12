-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 8
--
-- Week 8(04-08 Nov.)
-- Indexed data represented as a tree


module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT                  
                  )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = 1 + max (depth left) (depth right)

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node k a left right) = toList left ++ [(k,a)] ++ toList right

-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <= k  = Node k v (f left) right
                              | otherwise = Node k v left (f right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get _ Leaf = Nothing
get key (Node k v left right) | key == k  = Just v
                              | key <= k  = get key left
                              | otherwise = get key right

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList = foldr (uncurry set) Leaf


prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 13

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT _ Leaf = Leaf
filterLT key (Node k a left right) | k >= key  = filterLT key left
                                   | otherwise = Node k a left (filterLT key right) 

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT _ Leaf = Leaf
filterGT key (Node k a left right) | k <= key  = filterGT key right
                                   | otherwise = Node k a (filterGT key left) right

-- Exercise 14

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge Leaf Leaf                   = Leaf
merge (Node k a left right) Leaf  = (Node k a left right)
merge Leaf (Node k a left right)  = (Node k a left right)
merge (Node k a left right) tree2 = Node k a (merge left (filterLT k tree2)) (merge right (filterGT k tree2))

prop_merge :: (Ord k, Eq a) => Keymap k a -> Keymap k a -> Bool
prop_merge t1 t2 = and [(get key mergeT) == (Just val) | (key,val) <- toList t1] && and [(get key mergeT) == (Just val) | (key,val) <- toList t2] where
  mergeT = merge t1 t2

-- Exercise 15

del :: Ord k => k -> Keymap k a -> Keymap k a
del key Leaf = Leaf
del key (Node k a left right) | key == k  = merge left right
                              | key < k   = del key left
                              | otherwise = del key right  

-- Exercise 16

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select pred Leaf = Leaf
select pred (Node k a left right) | pred a    = Node k a (select pred left) (select pred right)
                                  | otherwise = select pred (del k (Node k a left right))

-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary