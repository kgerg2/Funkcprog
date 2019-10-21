isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = divisors n == []

iSqrt :: Int -> Int
iSqrt n = floor (sqrt (fromIntegral n))

divisors :: Int -> [Int]
divisors n = [x | x <- [2..iSqrt n], x `divides` n]

divides :: Int -> Int -> Bool
divides a b = mod b a == 0

primes :: [Int]
primes = 2:[x | x <- [3,5..], isPrime x]

pithagoreanTriples :: Int -> [(Int, Int, Int)]
pithagoreanTriples n = [(a, b, c) | a <- [1..n], b <- [a+1..n], c <- [b+1..iSqrt (a^2+b^2)], a^2 + b^2 == c^2]

[ n^2 | n <- [1..], even n ]
[2^x | x <- [0..10]] :: [Integer]
[2*x^2 | x <- [0..9]] :: [Integer]
[(-1)^x == 1 | x <- [1..10]]
head [2^x | x <- [0..], 2^x > 10^20]
head [n | n <- [0..], 1024^n > 2*1000^n]
[n | n <- [1..60], 60 `mod` n == 0]
length [n | n <- [1..60], 60 `mod` n == 0]
null [n | n<-[2..123457 `div` 2], 123457 `mod` n == 0]
[(o, p) | o <- [0..23], p <- [0..59]]
[(a, b) | a <- [0..9], b <- [a..9]]
[(a, b, c) | a <- [True, False], b <- [True, False], c <- [True, False], (a || (b && c)) /= ((a || b) && c)]
[(h, n) | h <- [1..12], n <- [1..[31,28,31,30,31,30,31,31,30,31,30,31]!!(h-1)]]
zip [1..] ['a'..'z']
[(b, a-b) | a <- [0..], b <- [0..a]]
[x | x <- [1..], _ <- [1..x]]
concat [[x | x <- [1..hossz-1]++[hossz,hossz-1..1], even hossz || x > 1] | hossz <- [2..]]
unwords [['*' | _ <- [1..hossz]] | hossz <- [1..]]
unwords [take hossz ['*','*'..] | hossz <- [1..]]
unwords [replicate hossz '*' | hossz <- [1..]]
[x | x <- [0..], floor (sqrt (fromIntegral x)) ^ 2 /= x]
[x | h <- [1..], x <- [1..h]]
concat [[x | x <- [0..hossz-1]++[hossz,hossz-1..0]] ++ [x | x <- [(-1),(-2)..(-hossz)+1]++[(-hossz)..(-1)]] | hossz <- [1..]]
4 * sum [1/fromIntegral x | x <- [(-1)^y*(2*y+1) | y <- [0..999]]]
