f :: Integer -> Integer
f x = 3*x^5 + 4*x^3 + 7*x^2 + 4

fValuesTo :: Integer -> [Integer]
fValuesTo x = [f x | x <- [0..x]]

main = print $ f 5