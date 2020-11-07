{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MonadComprehensions #-}
module Notes07 where

import Control.Monad (ap, forM, forM_, liftM2)
import Control.Monad.State
import Control.Applicative

-------------------------------------------------------------------------------

data BinTree a = Leaf a | Node (BinTree a) (BinTree a)
  deriving (Functor, Foldable, Traversable)

-- class Semigroup m where
--   (<>) :: m -> m -> m

-- class Semigroup m => Monoid m where
--   mempty :: m

-- instance Semigroup [] where (<>) = (++)
-- instance Monoid [] where mempty = []

-- class Foldable f where
--   foldMap :: Monoid m => (a -> m) -> f a -> m
--   foldr   :: (a -> b -> b) -> b -> f a -> b

foldMapList :: Monoid m => (a -> m) -> [a] -> m
foldMapList f []     = mempty
foldMapList f (x:xs) = f x <> foldMapList f xs

-- foldMapList f [x, y, z] == f x <> f y <> f z <> mempty == f x <> f y <> f z

-- Define foldMap using foldr
foldMapFromFoldr :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
-- foldMapFromFoldr f l = foldr (\x xs -> f x <> xs) mempty l
foldMapFromFoldr f l = foldr ((<>) . f) mempty l

-- Define the functions null', length' and toList' using foldr
null' :: Foldable f => f a -> Bool
null' = foldr (\_ _ -> False) True

length' :: Foldable f => f a -> Int
length' = foldr (\_ -> (+1)) 0

toList' :: Foldable f => f a -> [a]
toList' = foldr (:) []

-- Define the functions null'', length'' and toList'' using foldMap
-- Hint : you should define some monoid instances for Bool and Int

instance Semigroup Bool where
    (<>) = (&&)
instance Monoid Bool where
    mempty = True

null'' :: Foldable f => f a -> Bool
null'' = foldMap (\_ -> False)

instance Semigroup Int where
    (<>) = (+)
instance Monoid Int where
    mempty = 0

length'' :: Foldable f => f a -> Int
length'' = foldMap (\_ -> 1)

toList'' :: Foldable f => f a -> [a]
toList'' = foldMap (\x -> [x])

-- Define the following functions on Foldable
--   Try to use both foldr and foldMap

-- firstElem p xs should return the first element of xs that satisfies the predicate p. 
--   Just x means that this element was x.
--   Nothing means that there was no such element.
firstElem :: Foldable f => (a -> Bool) -> f a -> Maybe a
firstElem p = foldr (\x y -> if (p x) then Just x else Nothing <|> y) Nothing

-- lastElem p xs should return the last element of xs that satisfies the predicate p. 
lastElem :: Foldable f => (a -> Bool) -> f a -> Maybe a
lastElem p = foldr (\x y -> y <|> if (p x) then Just x else Nothing) Nothing

sum' :: (Num a, Foldable f) => f a -> a
sum' = foldr (+) 0

product' :: (Num a, Foldable f) => f a -> a
product' = foldr (*) 1

-- maximum' xs should return the maximum element of xs.
--   Nothing means that xs was empty.
maximum' :: (Ord a, Foldable f) => f a -> Maybe a
maximum' = foldr helper Nothing where
    helper :: Ord a => a -> Maybe a -> Maybe a
    helper x Nothing  = Just x
    helper x (Just y) = Just $ max x y

-- class Traversable f where
--   traverse :: Applicative m => (a -> m b) -> f a -> m (f b)

-- Bonus: define foldr using foldMap
-- foldrFromFoldMap :: (Foldable f) => (a -> b -> b) -> b -> f a -> b
-- foldrFromFoldMap f n l = foldMap (_) (toList' l)

-- Bonus: define foldMap using traverse
-- foldMapFromTraverse :: (Traversable f, Monoid m) => (a -> m) -> f a -> m
-- foldMapFromTraverse f l = traverse _ _

-- Bonus: define fmap using traverse
fmapFromTraverse :: (Traversable f) => (a -> b) -> f a -> f b
fmapFromTraverse = undefined

--------------------------------------------------------------------------------