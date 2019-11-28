import Data.List

type Cell = Integer
type Alive = Bool
type Interval = (Integer, Integer)
type Neighbourhood = (Alive, Alive, Alive)
type State = (Cell -> Alive)

initial :: Cell -> Alive
initial n = n == 0

takeInterval :: Interval -> (Cell -> Alive) -> [Alive]
takeInterval (a, b) f = map f [a..b]

rule51 :: Neighbourhood -> Alive
rule51 (_, b, _) = not b

rule150 :: Neighbourhood -> Alive
rule150 (a, b, c) = (a /= b) /= c

step :: (Neighbourhood -> Alive) -> State -> State
step f s n = f (s (n-1), s n, s (n+1))

run :: (Neighbourhood -> Alive) -> State -> [State]
run f s = s : run f (step f s)

displayCell :: Bool -> Char
displayCell True  = '#'
displayCell False = '_'

displayState :: Interval -> State -> String
displayState i s = map displayCell $ takeInterval i s

display :: Interval -> Int -> [State] -> String
display i n l = intercalate "\n" $ map (displayState i) (take n l)

main = putStr $ display (-5,5) 15 (run rule150 initial)