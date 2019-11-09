digits :: Integer -> [Integer]
digits x
  | x < 10    = [x]
  | otherwise = mod x 10 : digits (x `div` 10)

squareSum :: [Integer] -> Integer
squareSum l = sum [ x*x | x <- l]

happy :: Integer -> Bool

happy x = h x 0 where
    h :: Integer -> Integer -> Bool
    h 1 _    = True
    h _ 1000 = False
    h x db   = h (squareSum (digits x)) (db + 1)

happyNumbers :: [Integer]
happyNumbers = [ x | x <- [1..500], happy x ]