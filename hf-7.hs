lnko :: Integer -> Integer -> Integer
lnko a 0 = a
lnko a b = lnko b (a `mod` b)

type Numer = Integer
type Denom = Integer
type Ratio = (Numer, Denom)

simplifyRatio :: Ratio -> Ratio
simplifyRatio (n, d) = (n `div` lnko n d, d `div` lnko n d)

addRatios :: Ratio -> Ratio -> Ratio
addRatios (a, b) (c, d) = simplifyRatio (a*d + b*c, b*d)

multRatios :: Ratio -> Ratio -> Ratio
multRatios (a, b) (c, d) = simplifyRatio (a*c, b*d)