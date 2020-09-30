module Bead03 where

import Control.Monad

-- Bead assignment 03:
--   Define the function `f :: Maybe (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c`.
--   You can use the Monad instance for Maybe or the function bindMaybe if you want.
-- zipWithMaybe f as bs should behave like zipWith, but fail (return Nothing)
--   whenever `f a b` gives Nothing.

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing  _ = Nothing
bindMaybe (Just a) f = f a

f :: Maybe (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
f (Just g) (Just a) (Just b) = Just (g a b)
f _        _        _        = Nothing

zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
-- zipWithMaybe f (x:xs) (y:ys) = do
--     z <- f x y
--     zs <- zipWithMaybe f xs ys
--     return (z:zs)
zipWithMaybe f (x:xs) (y:ys) = liftM2 (:) (f x y) (zipWithMaybe f xs ys)
zipWithMaybe _ _ _ = return [] 


-- Examples:
-- zipWithMaybe (\a b -> Just (a + b)) [x,y] [z,w] = Just [x+z, y+w]
-- zipWithMaybe (\a b -> Nothing) [] [] == Just []
-- zipWithMaybe (\a b -> Nothing) [1] [2] == Nothing
-- zipWithMaybe (\a b -> if even (a+b) then Just (a+b) else Nothing) [2,1] [2,2] == Nothing
-- zipWithMaybe (\a b -> if even (a+b) then Just (a+b) else Nothing) [2,1] [2,1] == Just [4, 2]
