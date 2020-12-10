module Bead11 where

import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad

newtype State s a = State { runState :: s -> (s, a) }

execState :: State s a -> s -> s
execState (State f) s = fst (f s)

evalState :: State s a -> s -> a
evalState (State f) s = snd (f s)

put :: s -> State s ()
put s = State (\_ -> (s, ()))

get :: State s s
get = State (\s -> (s, s))

modify :: (s -> s) -> State s ()
modify f = State (\s -> (f s, ()))

instance Functor (State s) where 
  fmap f (State g) = State $ \s -> fmap f (g s)
instance Applicative (State s) where pure = return; (<*>) = ap
instance Monad (State s) where
  return x = State (\s -> (s, x))
  State f >>= g = State (\s -> let (s', a) = f s in runState (g a) s')
--------------------------------------------------------------------------------

-- Bead assignment 11:
--  Define a Traversable instance for `Tree`. (1pt)
--  Define the function `nthElem :: Int -> Tree a -> Maybe a`. (2pt)
--    `nthElem n t` should return `Nothing` if the index n is non-positive or too large.
--    Otherwise, `nthElem n t` should return `Just x`, where x is the (n-1)-th item in the tree t.

data Tree a = Leaf a 
            | Node (Tree a) [Tree a]
            deriving (Show, Eq, Ord)

tree1, tree2, tree3 :: Tree Int
tree1 = Leaf 0
tree2 = Node (Leaf 1) [Node (Leaf 2) [], Node (Leaf 3) []]
tree3 = Node (Node (Leaf 4) [Node (Leaf 5) [Node (Leaf 6) []]]) [Node (Leaf 7) []]

instance Functor Tree where
  fmap f (Leaf x)   = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap (fmap f) r)

instance Foldable Tree where
  foldMap f (Leaf x)   = f x
  foldMap f (Node l r) = foldMap f l <> foldMap (foldMap f) r

instance Traversable Tree where
  traverse f (Leaf x)   = Leaf <$> f x
  traverse f (Node l r) = Node <$> traverse f l <*> (fmap (traverse f) r)

nthElem :: Int -> Tree a -> Maybe a
nthElem = undefined

-- Examples:

label :: State Int Int
label = do n <- get; put (n+1); pure n

-- evalState (traverse (\_ -> label) tree1) 0 == Leaf 0
-- evalState (traverse (\_ -> label) tree2) 0 == Node (Leaf 0) [Node (Leaf 1) [], Node (Leaf 2) []]
-- evalState (traverse (\_ -> label) tree3) 0 == Node (Node (Leaf 0) [Node (Leaf 1) [Node (Leaf 2) []]]) [Node (Leaf 3) []]

-- nthElem 0 tree1 == Just 0
-- nthElem (-1) tree1 == Nothing
-- nthElem 1 tree1 == Nothing
-- nthElem 0 tree2 == Just 1
-- nthElem 1 tree2 == Just 2
-- nthElem 0 tree3 == Just 4
-- nthElem 1 tree3 == Just 5
