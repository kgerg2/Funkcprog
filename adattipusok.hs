data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show, Eq, Ord, Enum)

{-
instance Show Day where
  show Mon = "hétfő"

instance Enum Day where
  fromEnum Mon = 0
  fromEnum Sun = 6
  toEnum 0 = Mon
  succ d = toEnum $ (fromEnum d + 1) `mod` 7
-}

data Time = T Int Int
--  deriving Show

--Time x y = Time x y

instance Show Time where
--showTime Time -> String
  show (T a b) = (pad . show) a ++ ":" ++ (pad . show) b where
    pad xs = replicate (2 - length xs) '0' ++ xs

instance Eq Time where
  (T a b) == (T c d) = a == c && b == d

--data Maybe a = Just a | Nothing

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing


data List a = Nil | Cons a (List a)
  deriving Show

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x $ fromList xs
{-
data Deep l = D [Deep l]
  deriving Show
-}
