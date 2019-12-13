import Text.Printf

type Hour = Int
type Min  = Int
data Time = T Hour Min deriving (Eq, Show, Ord)
data Time12 = AM Int Int | PM Int Int deriving Eq

showTime12 :: Time12 -> String
showTime12 (AM h m) = printf "%02d.%02d am" h m
showTime12 (PM h m) = printf "%02d.%02d pm" h m

time12ToTime :: Time12 -> Time
time12ToTime (AM h m) = T (h `mod` 12) m
time12ToTime (PM h m) = T (h `mod` 12 + 12) m

timeToTime12 :: Time -> Time12
timeToTime12 (T h m)
  | h < 12 && h + m > 0 = AM h m
  | h < 12              = AM 12 00
  | h > 12 || m > 0     = PM (h-12) m
  | otherwise           = PM 12 00