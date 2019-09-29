isPrime :: Int -> Bool
isPrime n = length (divisors n) == 2

divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]
