piApprox :: Integer -> Double
piApprox n = 4 * sum [1/fromIntegral x | x <- [(-1)^y*(2*y+1) | y <- [0..n-1]]]

main = print $ piApprox 1
