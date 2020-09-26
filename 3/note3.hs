{-# LANGUAGE DeriveFunctor, MonadComprehensions #-}
module Notes03 where

import Control.Monad

-- class Monad f where
--   return :: a -> f a
--   (>>=) :: f a -> (a -> f b) -> f b
-- (>>=) is read "bind".

data Tree1 a = Leaf1 a
             | Node1 (Tree1 a) (Tree1 a)
             deriving(Show, Eq, Ord, Functor)
-- Functor can be derived
returnTree1 :: a -> Tree1 a
returnTree1 = Leaf1

bindList :: (a -> [b]) -> [a] -> [b]
-- bindList f l = [ y | x <- l, y <- f x]
-- bindList f [] = []
-- bindList f (x:xs) = (f x) ++ (bindList f xs)
bindList f l = concat $ map f l
-- example: bindList (\x -> [x, x+1]) [1, 2] == [1, 2, 2, 3]

bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe _ Nothing = Nothing
bindMaybe f (Just x) = f x
-- bindMaybe f = concatMaybe . mapf f

-- example:
  -- bindMaybe (\x -> if x then Nothing else Just (not x)) Nothing      == Nothing
  -- bindMaybe (\x -> if x then Nothing else Just (not x)) (Just True)  == Nothing
  -- bindMaybe (\x -> if x then Nothing else Just (not x)) (Just False) == Just True

bindTree1 :: (a -> Tree1 b) -> Tree1 a -> Tree1 b
bindTree1 f (Leaf1 x) = f x
bindTree1 f (Node1 x y) = Node1 (bindTree1 f x) (bindTree1 f y)

instance Applicative Tree1 where
  pure = return
  (<*>) = ap

instance Monad Tree1 where
  return = returnTree1
  t >>= f = bindTree1 f t

tree1 :: Tree1 Int
tree1 = bindTree1 
        (\x -> if x then Leaf1 0 else Node1 (Leaf1 0) (Leaf1 1))
        (Node1 (Leaf1 True) (Leaf1 False))

tree1' :: Tree1 Int
tree1' = Node1 (Leaf1 0) (Node1 (Leaf1 0) (Leaf1 1))

-- tree1 == tree1'



concatList :: [[a]] -> [a]
concatList l = foldr (++) [] l

concatMaybe :: Maybe (Maybe a) -> Maybe a
-- concatMaybe Nothing  = Nothing
-- concatMaybe (Just x) = x
concatMaybe = bindMaybe id

concatTree1 :: Tree1 (Tree1 a) -> Tree1 a
-- concatTree1 = join
concatTree1 (Leaf1 x) = x
concatTree1 (Node1 x y) = Node1 (concatTree1 x) (concatTree1 x)

tree2 :: Tree1 (Tree1 Int)
tree2 = Node1 (Leaf1 (Node1 (Leaf1 0) (Leaf1 2))) (Leaf1 (Leaf1 3))

-- concatTree1 tree2 == Node1 (Node1 (Leaf1 0) (Leaf1 2)) (Leaf1 3)


sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe [] = Just []
-- sequenceMaybe (x:xs) = case (x, sequenceMaybe xs) of
--     (Just y, Just ys) -> Just (y:ys)
--     -> Nothing

-- sequenceMaybe (x:xs) = (:) <$> x <*> sequenceMaybe xs
sequenceMaybe (x:xs) = apMaybe (fmap (:) x) (sequenceMaybe xs)


-- sequenceMaybe l = 
-- sequenceMaybe [] = Just []
-- sequenceMaybe (Nothing:_) = Nothing
-- sequenceMaybe ((Just x):xs) = l
--   where
--     l = sequenceMaybe xs



--   | isNothing l = Nothing
--   | otherwise   = Just (x:l) where
--       Just l = sequenceMaybe xs
    -- --   l = Nothing
    --   isNothing Nothing = True
    --   isNothing _       = False

-- sequenceTree1 :: [Tree1 a] -> Tree1 [a]
-- sequenceTree1 l = 


traverseTree1_Maybe :: (a -> Maybe b) -> Tree1 a -> Maybe (Tree1 b)
traverseTree1_Maybe = undefined

traverseList_Maybe :: (a -> Maybe b) -> [a] -> Maybe [b]
traverseList_Maybe = undefined

apList :: [a -> b] -> [a] -> [b]
-- apList = (<*>)
-- apList [] _     = []
-- apList (f:fs) l = map f l ++ apList fs l

apList fs ys = fs >>= \f -> map f ys
apList fs ys = do
  f <- fs
  y <- ys
  return (f y)
-- apList (f:fs) l = map 
-- example:
--   apList [ (*2), (*3), (*5) ] [ 1, 7 ] == [ 1, 14, 3, 21, 5, 35 ]

apMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
apMaybe = undefined
