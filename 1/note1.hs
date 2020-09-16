module Notes01 where

-- GHCi commands:
--  :l File.hs    Load a file
--  :r            Reload the loaded file
--  expr          Evaluate the expression `expr`
--  :t expr       Display the type of the expression `expr`
--  :i expr       Display information about the name `expr`

-- Typed holes
--   (In vscode: press F8 to display the expected type of a hole, and the types of the variables in scope)
--   (In GHCi: reload to see the same information)

f0 :: ((a, b) -> c) -> b -> a -> c
f0 g a b = g (b, a)

f1 :: (a -> b) -> a -> (b, a)
f1 p a = (p a, a)

-- Algebraic data types

-- (a, b) in Prelude
data Pair a b = Pair a b

-- Either a b in Prelude
data Either' a b = Left' a
                 | Right' b
                 deriving (Show)

-- Maybe a in Prelude
data Maybe' a = Just' a 
              | Nothing'
              deriving (Show)

-- [a] in Prelude
data List a = Empty 
            | Cons a (List a)
            deriving (Show)

data BinTree a = Leaf a
               | Node (BinTree a) (BinTree a)
               deriving (Show)

-- Show, Eq and Ord instances

-- class Show a where
--   show :: a -> String

-- class Eq a where
--   (==) :: a -> a -> Bool

-- class Eq a => Ord a where
--   (<=) :: a -> a -> Bool

instance (Show a, Show b) => Show (Pair a b) where
--   show :: Pair a b -> String
  show (Pair a b) = "(" ++ show a ++ ", " ++ show b ++ ")"

instance (Eq a, Eq b) => Eq (Pair a b) where
  (Pair a b) == (Pair c d) = a == c && b == d

instance (Ord a, Ord b) => Ord (Pair a b) where
  (Pair a b) <= (Pair c d) = a <= c || (a == c && b <= d)

instance (Eq a, Eq b) => Eq (Either' a b) where
  (Left' a)  == (Left' b)  = a == b
  (Right' a) == (Right' b) = a == b
  _          == _          = False

instance Eq a => Eq (Maybe' a) where
  Just' a  == Just' b  = a == b
  Nothing' == Nothing' = True
  _        == _        = False

instance Eq a => Eq (List a) where
  Empty == Empty = True
  Cons a as == Cons b bs = a == b && as == bs
  _         == _         = False

instance Eq a => Eq (BinTree a) where
  Leaf x   == Leaf y   = x == y
  Node a b == Node c d = a == b && c == d
  _        == _        = False

-- map in Prelude
mapList :: (a -> b) -> List a -> List b
mapList _ Empty       = Empty
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

mapMaybe :: (a -> b) -> Maybe' a -> Maybe' b
mapMaybe _ Nothing'  = Nothing'
mapMaybe f (Just' a) = Just' (f a)

mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Leaf a)   = Leaf (f a)
mapTree f (Node a b) = Node (mapTree f a) (mapTree f b)
