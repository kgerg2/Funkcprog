module Notes02 where

mapList :: (a -> b) -> [a] -> [b]
mapList f = foldr (\x-> (f x :)) []
-- mapList _ [] = []
-- mapList f (x:xs) = f x : mapList f xs

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

mapPair :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapPair f g (a, b) = (f a, g b)

mapEither :: (a -> b) -> (c -> d) -> Either a c -> Either b d
mapEither f _ (Left a)  = Left (f a)
mapEither _ f (Right a) = Right (f a)

f :: (a -> b) -> [[a]] -> [[b]]
f = mapList . mapList
-- f _ []     = []
-- f g (x:xs) = mapList g x : f g xs

g :: (a -> b) -> [(a, a)] -> [(a, b)]
g = mapList . mapPair id
-- g f = map (\(x, y) -> (x, f y))
-- g _ [] = []
-- g f ((x, y):xs) = (x, f y) : g f xs 

data Tree1 a = Leaf1 a
             | Node1 (Tree1 a) (Tree1 a)

mapTree1 :: (a -> b) -> Tree1 a -> Tree1 b
mapTree1 f (Leaf1 x)   = Leaf1 (f x)
mapTree1 f (Node1 x y) = Node1 (mapTree1 f x) (mapTree1 f y)

data Tree2 a = Leaf2 a
             | Node2 [Tree2 a]

mapTree2 :: (a -> b) -> Tree2 a -> Tree2 b
mapTree2 f (Leaf2 x) = Leaf2 (f x)
mapTree2 f (Node2 x) = Node2 (mapList (mapTree2 f) x)

data Tree3 a = Leaf3 a
             | Node3 (Int -> [Tree3 a])

mapTree3 :: (a -> b) -> Tree3 a -> Tree3 b
mapTree3 f (Leaf3 x) = Leaf3 (f x)
mapTree3 f (Node3 g) = Node3 ((map $ mapTree3 f) . g) 

concatList :: [[a]] -> [a]
concatList l = foldr (++) [] l

concatMaybe :: Maybe (Maybe a) -> Maybe a
concatMaybe Nothing  = Nothing
concatMaybe (Just x) = x

concatTree1 :: Tree1 (Tree1 a) -> Tree1 a
concatTree1 (Leaf1 x) = x
concatTree1 (Node1 x y) = Node1 (concatTree1 x) (concatTree1 x)

bindTree1 :: (a -> Tree1 b) -> Tree1 a -> Tree1 b
bindTree1 f = concatTree1 . mapTree1 f
-- bindTree1 f (Leaf1 x) = f x
-- bindTree1 f (Node1 x y) = Node1 (bindTree1 f x) (bindTree1 f y)

bindList :: (a -> [b]) -> [a] -> [b]
bindList f = concatList . map f

bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe f = concatMaybe . mapMaybe f
-- bindMaybe _ Nothing = Nothing
-- bindMaybe f (Just x) = f x

mapFun :: (a -> b) -> (Int -> a) -> (Int -> b)
mapFun f x = f . x