import Data.Char
import Data.List

sortTuple :: Ord a => (a, a) -> (a, a)
sortTuple t@(a, b)
  | a <= b    = t
  | otherwise = (b, a)

caseSwap :: Char -> Char
caseSwap c
  | isLower c = toUpper c
  | isUpper c = toLower c
  | otherwise = c

count :: Eq a => a -> [a] -> Int
count x l = length $ filter (==x) l

listMul :: [Int] -> [Int] -> Int
listMul l = sum . zipWith (*) l

sameSign :: [Int] -> Bool
sameSign [] = True
sameSign l = (maximum l) * (minimum l) >= 0

isCorrect :: [(Int, Int)] -> Bool
isCorrect [] = True
isCorrect l@(x:xs) = and $ zipWith (\ (_, a) (b, _) -> a==b) l xs

filterMany :: [a -> Bool] -> [a] -> [a]
filterMany f l = filter (\x -> foldr (\a b -> a x && b) True f) l

conditionalMax :: Ord a => (a -> Bool) -> [a] -> Maybe a
conditionalMax f l
  | null fl = Nothing
  | otherwise = Just $ maximum fl where
    fl = filter f l

data Season = Winter | Spring | Summer | Autumn
  deriving (Eq, Ord)

nextSeason :: Season ->  Season
nextSeason Autumn = Winter
nextSeason Winter = Spring
nextSeason Spring = Summer
nextSeason Summer = Autumn

seasonAfterMonths :: Int -> Season
seasonAfterMonths i = (iterate nextSeason Winter) !! ((i+1) `div` 3)

removeSpecial :: String -> String
removeSpecial s = filter (`elem` (' ' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])) s

isSublist :: Eq a => [a] -> [a] -> Bool
isSublist [] []      = True
isSublist _  []      = False
isSublist s l@(x:xs) = isPrefixOf s l || isSublist s xs

multipleElems :: Eq a => [a] -> [a]
multipleElems l = reverse $ h [] l where
    h a [] = a
    h a (x:xs)
        | x `elem` xs && not (x `elem` a) = h (x:a) xs
        | otherwise                       = h a xs
        
maxTempChange :: [(Int, Int)] -> Int
maxTempChange l = snd . maximum . map (\((a, b), i) -> (b-a, i)) $ zip l [1..]

primeIndex :: [a] -> [a]
primeIndex l = map fst $ h primes $ zip l [1..] where
    h _         []     = []
    h pl@(p:ps) (x:xs)
        | snd x == p = x : h ps xs
        | otherwise  = h pl xs
    primes = filter isPrime [1..]
    isPrime :: Int -> Bool
    isPrime n | n == 2    = True
              | n > 2     = null $ filter ((==0) . (n `mod`)) (2:[3, 5..n-1])
              | otherwise = False

