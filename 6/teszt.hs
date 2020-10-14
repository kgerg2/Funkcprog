module Bead05 where

import Data.Maybe
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

-- Bead assignment 05:
-- Give a "translation" of the following imperative program using the Haskell state monad:

-- (a, b) = (0, 0)
-- for i from 1 to n do
--   for j from 1 to i do
--     (a, b) = (b + i, a + j)

p :: Integer -> State (Integer, Integer) ()
p n = do
    put (0, 0)
    forM_ [1..n] $ \i ->
        forM_ [1..i] $ \j ->
            modify (\(a, b) -> (b + i, a + j))

runP :: Integer -> (Integer, Integer)
runP n = execState (p n) (0, 0)

-- Examples:
-- runP 0 == (0, 0)
-- runP 1 == (1, 1)
-- runP 2 == (4, 5)
-- runP 3 == (13, 11)
-- fmap runP [0..10] == [(0,0),(1,1),(4,5),(13,11),(25,25),(46,44),(73,74),(114,110),(162,162),(227,223),(302,303)]
