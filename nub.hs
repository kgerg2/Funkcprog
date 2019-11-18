{-

myNub :: Eq a => [a] -> [a]
myNub l = nub_ [] l where
  nub_ l [] = reverse l
  nub_ l (x:xs)
    | elem x l  = nub_ l     xs
    | otherwise = nub_ (x:l) xs

-}

myNub :: Eq a => [a] -> [a]
myNub l = nub_ [] l where
  nub_ l [] = []
  nub_ l (x:xs)
    | elem x l  = nub_ l     xs
    | otherwise = x : nub_ (x:l) xs
