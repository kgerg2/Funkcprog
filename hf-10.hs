import Data.Char
import Data.Maybe
import Data.List

toBitString 0 = []
toBitString n = (n `mod` 2) : toBitString (n `div` 2)

charToBitString c = take 7 $ toBitString (ord c) ++ [0, 0..]

-- bitStringToChar l = chr $ bitStringToInt l where
--     bitStringToInt [] = 0
--     bitStringToInt (x:xs) = x + 2 * bitStringToInt xs
bitStringToChar l = chr . sum $ zipWith (*) l (iterate (*2) 1)

xor [] l = l
xor l [] = l
xor (x:xs) (y:ys) = (x + y) `mod` 2 : xor xs ys

encrypt s k = zipWith (\a b -> bitStringToChar $ xor (charToBitString a) (charToBitString b)) s (cycle k)

decrypt = encrypt

isCycledIn [] _ = error "empty list"
isCycledIn a b = length a < length b && and (zipWith (==) b (cycle a))

getKey c s = fromJust $ find (`isCycledIn` l) (drop 1 $ inits l) where
    l = decrypt c s

decodeMessage c s = decrypt c $ getKey (take (length s) c) s