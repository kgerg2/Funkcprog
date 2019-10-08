isLeapYear :: Int -> Bool
isLeapYear x = x `mod` 4 == 0 && (x `mod` 100 /= 0 || x `mod` 400 == 0)

monthDayPairs :: Int -> [(Int, Int)]
monthDayPairs ev = [(h,n) | h<-[1..12], n<-[1..31], (not (h `elem` [4, 6, 9, 11]) || n <= 30) && (h /= 2 || n <= 28 || (isLeapYear ev && n == 29))]