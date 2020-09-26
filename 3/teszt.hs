module Bead02 where

-- Bead assignment 02:
--   Define an `Functor` instance for `T`.

data T a = MkT Bool a [a] 
         deriving (Show, Eq)

mapT :: (a -> b) -> T a -> T b
mapT f (MkT b a l) = MkT b (f a) (map f l)

instance Functor T where
  fmap = mapT

-----------
-- Tests --
-----------

-- mapT id (MkT False 0 [1, 2, 3]) == (MkT False 0 [1, 2, 3])
-- mapT (+ 1) (MkT False 0 [1, 2, 3]) == (MkT False 1 [2, 3, 4])
-- mapT (* 2) (MkT True 5 []) == (MkT True 10 [])
-- mapT ('a':) (MkT True "b" ["c", "d"]) == (MkT True "ab" ["ac", "ad"])
