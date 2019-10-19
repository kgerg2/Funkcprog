isPerfectSquare :: Integer -> Bool
isPerfectSquare n = round (sqrt (fromIntegral n)) ^ 2 == n

pythagoreanTriples :: Integer -> [(Integer, Integer, Integer)]
pythagoreanTriples n = [(a, b, round (sqrt (fromIntegral (a^2 + b^2)))) | a <- [2..n], b <- [a..n], isPerfectSquare (a^2 + b^2), round (sqrt (fromIntegral (a^2 + b^2))) <= n]