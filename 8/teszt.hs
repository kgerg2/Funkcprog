{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Bead06 where

import Control.Monad

--------------------------------------------------------------------------------
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

-- Bead assignment 06:
--  Define a function `sumLeft :: BinTree Integer -> BinTree Integer`.
--   `sumLeft t` should replace the value of each leaf `Leaf x` of 
--   the tree `t` with the sum of the values the leaves of `t` that 
--   are to the left of the leaf `Leaf x`.

data BinTree a = Leaf a
               | Node (BinTree a) (BinTree a)
               deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
-- Examples:
--   sumLeft (Leaf 10) 
--         == Leaf 10
--   sumLeft (Node (Leaf 10) (Leaf 2))
--         == Node (Leaf 10) (Leaf 12)
--   sumLeft (Node (Node (Leaf 1000) (Leaf 200))  (Node (Leaf 3)   (Leaf 40)))
--         == Node (Node (Leaf 1000) (Leaf 1200)) (Node (Leaf 1203) (Leaf 1243))

sumLeft :: BinTree Integer -> BinTree Integer
sumLeft t = evalState (sumLeft_s t) 0 where
  sumLeft_s (Leaf n)   = Leaf <$> sumNode n
  sumLeft_s (Node l r) = Node <$> sumLeft_s l <*> sumLeft_s r
  sumNode n = do
      i <- get
      put (i + n)
      pure (i + n)
