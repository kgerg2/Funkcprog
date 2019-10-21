{-fact n = product [1..n]

fact n
  | n == 0 = 1
  | True   = n * fact (n-1)
-}

fact :: Integer -> Integer
fact 0 = 1
fact n
  | n > 0 = n * fact (n-1)


-- Végrekurzív:

fact1 :: Integer -> Integer
fact1 n = factH n 1 1 where
  factH n m k
    | n == m = k * n
    | m <  n = factH n (m+1) (k*m)
