module Bead10 where

import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.State

--------------------------------------------------------------------------------

-- Bead assignment 10:
--  Define Functor, Foldable and Traversable instances for the following type.

data Tree a = Leaf a a
            | Node1 (Maybe (Tree a))
            | Node2 (Tree a) (Tree a)
            deriving (Show, Eq, Ord)

instance Functor Tree where
  fmap f (Leaf a b)       = Leaf (f a) (f b)
--   fmap f (Node1 (Just t)) = Node1 (Just (fmap f t))
--   fmap _ (Node1 _)        = Node1 Nothing
  fmap f (Node1 a)        = Node1 (fmap (fmap f) a)
  fmap f (Node2 t1 t2)    = Node2 (fmap f t1) (fmap f t2)

instance Foldable Tree where
  -- Define either foldr or foldMap
  foldr f a (Leaf b c)       = f c (f b a)
  foldr f a (Node1 (Just t)) = foldr f a t
  foldr f a (Node1 _)        = error "Cannot fold Nothing"
  foldr f a (Node2 t1 t2)    = foldr f (foldr f a t1) t2
  -- foldMap = undefined

instance Traversable Tree where
  traverse f (Leaf a b)       = Leaf <$> f a <*> f b
  traverse f (Node1 (Just t)) = Node1 <$> Just <$> traverse f t
  traverse _ (Node1 _)        = pure $ Node1 Nothing
  traverse f (Node2 t1 t2)    = Node2 <$> traverse f t1 <*> traverse f t2
--------------------------------------------------------------------------------

-- Examples:
tree1 :: Tree Int
tree1 = Node2 (Leaf 1 2) (Node1 (Just (Leaf 3 4)))

tree2 :: Tree Int
tree2 = Node2 (Node1 Nothing) (Node1 (Just (Leaf 10 20)))

testsFmap :: [Bool]
testsFmap = [ fmap (+1) tree1 == Node2 (Leaf 2 3) (Node1 (Just (Leaf 4 5)))
            , fmap (+1) tree2 == Node2 (Node1 Nothing) (Node1 (Just (Leaf 11 21)))
            ]

testsFold :: [Bool]
testsFold = [ foldr (+) 0 tree1  == 10
            , foldr (+) 0 tree2  == 30
            , foldr (:) [] tree1 == [1,2,3,4]
            , foldr (:) [] tree2 == [10,20]
            ]

goLabel :: a -> State Int (a, Int)
goLabel a = do n <- get; put (n+1); pure (a,n)

testsTraverse :: [Bool]
testsTraverse = [ evalState (traverse goLabel tree1) 0
                  == Node2 (Leaf (1,0) (2,1)) (Node1 (Just (Leaf (3,2) (4,3))))
                , evalState (traverse goLabel tree2) 0
                  == Node2 (Node1 Nothing) (Node1 (Just (Leaf (10,0) (20,1))))
                ]
