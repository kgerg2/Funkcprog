import Data.Char
import Data.Maybe
import Data.List

type BitString = [Int]
type Cipher = String
type Key = String

toBitString :: Int -> BitString
toBitString 0 = []
toBitString n = (n `mod` 2) : toBitString (n `div` 2)

charToBitString :: Char -> BitString
charToBitString c = take 7 $ toBitString (ord c) ++ [0, 0..]

bitStringToChar :: BitString -> Char
bitStringToChar l = chr . sum $ zipWith (*) l (iterate (*2) 1)

xor :: BitString -> BitString -> BitString
xor [] l = l
xor l [] = l
xor (x:xs) (y:ys) = (x + y) `mod` 2 : xor xs ys

encrypt :: String -> Key -> Cipher
encrypt s k = zipWith (\a b -> bitStringToChar $ xor (charToBitString a) (charToBitString b)) s (cycle k)

decrypt :: Cipher -> Key -> String
decrypt = encrypt

isCycledIn :: Eq a => [a] -> [a] -> Bool
isCycledIn [] _ = error "empty list"
isCycledIn a b = length a < length b && and (zipWith (==) b (cycle a))

getKey :: Cipher -> String -> Key
getKey c s = fromJust $ find (`isCycledIn` l) (drop 1 $ inits l) where
    l = decrypt c s

decodeMessage :: Cipher -> String -> String
decodeMessage c s = decrypt c $ getKey (take (length s) c) s