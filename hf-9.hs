vector_length :: (Double, Double, Double) -> Double
vector_length (a, b, c) = sqrt (a^2 + b^2 + c^2)

headToBack :: [Int] -> [Int]
headToBack [] = []
headToBack [x] = [x]
headToBack (x:xs) = init xs ++ [x]

divModEq :: Int -> Int -> Bool
divModEq a b = a `mod` b == a `div` b

quadrant :: (Int, Int) -> Int
quadrant (x, y)
  | x > 0 && y > 0 = 1
  | x < 0 && y > 0 = 2
  | x < 0 && y < 0 = 3
  | x > 0 && y < 0 = 4
  | otherwise      = 0

pair_sums :: [Int] -> [Int]
pair_sums (x:y:xs) = (x+y):pair_sums xs
pair_sums _        = []

deliveryCost :: [(String, Double, Int)] -> Int
deliveryCost [] = 0
deliveryCost l
  | maximum (map (\ (_, x, _) -> x) l) >= 50 = 0
  | sum (map (\ (_, _, x) -> x) l) >= 30000  = 5000
  | otherwise                                = 10000

insert ::  Int -> Int -> [(Int, Int)] -> [(Int, Int)]
insert a b [] = [(a, b)]
insert a b l@(p@(x, y):xs)
  | b >= y    = (a, b):l
  | otherwise = p : insert a b xs