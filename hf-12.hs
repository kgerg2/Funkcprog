type Pos = (Int, Int)
type Cell = (Pos, Int)
type Sudoku = [Cell]
type Block = Int

numsInRow :: Sudoku -> Int -> [Int]
numsInRow s i = map snd $ filter (\((x, _), _) -> i == x) s

numsInCol :: Sudoku -> Int -> [Int]
numsInCol s i = map snd $ filter (\((_, x), _) -> i == x) s

posToBlock :: Pos -> Block
posToBlock (x, y) = x - x `mod` 3 + y `div` 3

blockToPositions x
  | x >= 0 && x < 9 = [(a, b) | a <- [xd..xd+2], b <- [xm..xm+2]]
  | otherwise       = error $ "bad block number " ++ show x where
    xd = x `div` 3 * 3
    xm = x `mod` 3 * 3

numsInBlock :: Sudoku -> Block -> [Int]
numsInBlock s b = map snd $ filter ((b ==) . posToBlock . fst) s

allUnique :: Eq a => [a] -> Bool
-- allUnique = not . or . fst . foldr (\x (b, l) -> (elem x l : b, x:l)) ([], [])
-- allUnique = not . any fst . foldr (\x xs -> (elem x $ map snd xs, x):xs) []
allUnique [] = True
allUnique (x:xs) = (not $ x `elem` xs) && allUnique xs

isSudokuPuzzle :: Sudoku -> Bool
isSudokuPuzzle s = all (\((x, y), a) -> x < 9 && x >= 0 && y < 9 && y >= 0 && a <= 9 && a >= 1) s &&
                   all (allUnique . ($ s)) [(`f` x) | f <- [numsInRow, numsInCol, numsInBlock], x <- [0..8]]
-- isSudokuPuzzle s = all (\((x, y), a) -> x < 9 && x >= 0 && y < 9 && y >= 0 && a <= 9 && a >= 1) s &&
--                    and [allUnique (numsInRow s x) && allUnique (numsInCol s x) && allUnique (numsInBlock s x) | x <- [0..8]]

isFilled :: Sudoku -> Bool
-- isFilled s = length s == 81 && allUnique (map fst s)
isFilled = flip all [(81==) . length, allUnique . map fst] . flip ($)

isSolved :: Sudoku -> Bool
-- isSolved s = isFilled s && isSudokuPuzzle s
isSolved = flip all [isFilled, isSudokuPuzzle] . flip ($)

isBlank :: Sudoku -> Pos -> Bool
isBlank s p = not . elem p $ map fst s

blankPositions :: Sudoku -> [Pos]
blankPositions s = filter (not . (`elem` map fst s)) [(x, y) | x <- [0..8], y <- [0..8]]

possibleNumsOnPos :: Sudoku -> Pos -> [Int]
possibleNumsOnPos s p = [x | x <- [1..9], isSudokuPuzzle $ (p, x):s, not . elem p $ map fst s]

possibleNumsForBlankPos :: Sudoku -> [(Pos, [Int])]
possibleNumsForBlankPos s = [(p, possibleNumsOnPos s p) | p <- blankPositions s]

hasSolution :: [(Pos, [Int])] -> Bool
-- hasSolution s = all (not . null . snd) s && (not . null) s
hasSolution = flip all [all (not . null . snd), not . null] . flip ($)

uniqueNumForBlankPos :: [(Pos, [Int])] -> [(Pos, Int)]
uniqueNumForBlankPos = map (\(p, [x]) -> (p, x)) . filter ((1==) . length . snd)

insertElem :: Sudoku -> Pos -> Int -> Sudoku
insertElem s p i
  | not $ isBlank s p = error $ "position " ++ show p ++ " is not blank"
  | otherwise         = (p, i):s

step :: Sudoku -> [Sudoku]
step s
  | not $ hasSolution p = []
  | isSolved s          = [s]
  | not $ null u        = [head u : s]
  | otherwise           = map ((:s) . (,) pp) pl where
    p = possibleNumsForBlankPos s
    (pp, pl) = head p
    u = take 1 $ uniqueNumForBlankPos p

solve :: Sudoku -> [Sudoku]
solve s
  | not $ isSudokuPuzzle s = error "improper sudoku"
  | isSolved s             = [s]
  | otherwise              = concatMap solve $ step s

sudoku :: Sudoku
sudoku = [((0,0),3),((0,1),6),((0,4),7),((0,5),1),((0,6),2),
          ((1,1),5),((1,6),1),((1,7),8),
          ((2,2),9),((2,3),2),((2,5),4),((2,6),7),
          ((3,4),1),((3,5),3),((3,7),2),((3,8),8),
          ((4,0),4),((4,3),5),((4,5),2),((4,8),9),
          ((5,0),2),((5,1),7),((5,3),4),((5,4),6),
          ((6,2),5),((6,3),3),((6,5),8),((6,6),9),
          ((7,1),8),((7,2),3),((7,7),6),
          ((8,2),7),((8,3),6),((8,4),9),((8,7),4),((8,8),3)]

sudoku2 :: Sudoku
sudoku2 = [((0,0),5),((0,1),3),((0,4),7),
           ((1,0),6),((1,3),1),((1,4),9),((1,5),5),
           ((2,1),9),((2,2),8),((2,7),6),
           ((3,0),8),((3,4),6),((3,8),3),
           ((4,0),4),((4,3),8),((4,5),3),((4,8),1),
           ((5,0),7),((5,4),2),((5,8),6),
           ((6,1),6),((6,6),2),((6,7),8),
           ((7,3),4),((7,4),1),((7,5),9),((7,8),5),
           ((8,4),8),((8,7),7),((8,8),9)]

main = print $ solve $ drop 2 sudoku2