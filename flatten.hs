flatten :: [(Int, Int)] -> [Int]
flatten l = concat [[a, b] | (a, b) <- l]
