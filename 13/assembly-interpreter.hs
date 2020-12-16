{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Data.Traversable
import Control.Applicative
import Control.Monad
import Data.String
import Data.Maybe
import Debug.Trace

newtype State s a = State {runState :: s -> (a, s)} deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))
  State f >>= g = State (\s -> case f s of (a, s') -> runState (g a) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

data ProgState = ProgState {
  r1     :: Int,
  r2     :: Int,
  r3     :: Int,
  cmp    :: Ordering,
  memory :: [Int]
  } deriving (Eq, Show)

startState :: ProgState
startState = ProgState 0 0 0 EQ (replicate 10 0)

type Label = String  -- címke a programban, ahová ugrani lehet

data Register
  = R1
  | R2
  | R3
  deriving (Eq, Show)

data Destination
  = DstReg Register     -- regiszterbe írunk
  | DstDeref Register   -- memóriába írunk, az adott regiszterben tárolt index helyére
  deriving (Eq, Show)

data Source
  = SrcReg Register     -- regiszterből olvasunk
  | SrcDeref Register   -- memóriából olvasunk, az adott regiszterben tárolt index helyéről
  | SrcLit Int          -- szám literál
  deriving (Eq, Show)

data Instruction
  = Mov Destination Source   -- írjuk a Destination-be a Source értékét
  | Add Destination Source   -- adjuk a Destination-höz a Source értékét
  | Mul Destination Source   -- szorozzuk a Destination-t a Source értékével
  | Sub Destination Source   -- vonjuk ki a Destination-ből a Source értékét
  | Cmp Source Source        -- hasonlítsunk össze két Source értéket `compare`-el, az eredményt
                             -- írjuk a `cmp` regiszterbe

  | Jeq Label                -- Ugorjunk az adott címkére ha a `cmp` regiszterben `EQ` van
  | Jlt Label                -- Ugorjunk az adott címkére ha a `cmp` regiszterben `LT` van
  | Jgt Label                -- Ugorjunk az adott címkére ha a `cmp` regiszterben `GT` van
  deriving (Eq, Show)

type RawProgram = [Either Label Instruction]

-- Beírunk r1-be 10-et, r2-be 20-at
p1 :: RawProgram
p1 = [
  Left "start",
  Right $ Mov (DstReg R1) (SrcLit 10),
  Left "l1",                            -- tehetünk bárhova címkét, nem muszáj használni a programban
  Right $ Mov (DstReg R2) (SrcLit 20)
  ]

-- Kiszámoljuk 10 faktoriálisát, az eredményt r2-ben tároljuk
p2 :: RawProgram
p2 = [
  Left "start",
  Right $ Mov (DstReg R1) (SrcLit 10),
  Right $ Mov (DstReg R2) (SrcLit 1),
  Left "loop",
  Right $ Mul (DstReg R2) (SrcReg R1),
  Right $ Sub (DstReg R1) (SrcLit 1),
  Right $ Cmp (SrcReg R1) (SrcLit 0),
  Right $ Jgt "loop"
  ]

-- Feltöltjük 0-9-el a memóriát
p3 :: RawProgram
p3 = [
  Left "start",
  Right $ Mov (DstDeref R1) (SrcReg R1),
  Right $ Add (DstReg R1) (SrcLit 1),
  Right $ Cmp (SrcReg R1) (SrcLit 10),
  Right $ Jlt "start"
  ]

type Program = [(Label, [Instruction])]

toProgram :: RawProgram -> Program
toProgram r = go (reverse r) [] [] where
  go :: RawProgram -> Program -> [Instruction] -> Program
  go []          p _  = p
  go (Left  l:r) p is = go r ((l, is):p) is
  go (Right i:r) p is = go r p (i:is)

type M a = State ProgState a

getReg :: Register -> ProgState -> Int
getReg R1 = r1
getReg R2 = r2
getReg R3 = r3

putReg :: Register -> ProgState -> Int -> ProgState
putReg R1 p n = ProgState n (r2 p) (r3 p) (cmp p) (memory p)
putReg R2 p n = ProgState (r1 p) n (r3 p) (cmp p) (memory p)
putReg R3 p n = ProgState (r1 p) (r2 p) n (cmp p) (memory p)

putMem :: Int -> ProgState -> Int -> ProgState
putMem i p n = ProgState (r1 p) (r2 p) (r3 p) (cmp p) (modifyAt (memory p) i n) where
  modifyAt :: [a] -> Int -> a -> [a]
  modifyAt (_:xs) 0 y = y:xs 
  modifyAt (x:xs) n y | n > 0 = x:modifyAt xs (n-1) y
  modifyAt _      _ _ = error "Index not found in memory."

getSrc :: Source -> ProgState -> Int
getSrc (SrcReg   r) p = getReg r p
getSrc (SrcDeref r) p = memory p !! getReg r p
getSrc (SrcLit   n) _ = n

getDst :: Destination -> ProgState -> Int
getDst (DstReg   r) = getSrc $ SrcReg r
getDst (DstDeref r) = getSrc $ SrcDeref r

putDst :: Destination -> ProgState -> Int -> ProgState
putDst (DstReg   r) p = putReg r p
putDst (DstDeref r) p = putMem (getReg r p) p

putCmp :: Ordering -> ProgState -> ProgState
putCmp o p = ProgState (r1 p) (r2 p) (r3 p) o (memory p)

lookupErr :: Label -> a
lookupErr l = error $ "Cannot find label " ++ l

eval :: Program -> [Instruction] -> M ()
eval _  []           = pure ()
eval pr (Mov d s:is) = modify (\p -> putDst d p (getSrc s p)) *> eval pr is
eval pr (Add d s:is) = modify (\p -> putDst d p (getDst d p + getSrc s p)) *> eval pr is
eval pr (Mul d s:is) = modify (\p -> putDst d p (getDst d p * getSrc s p)) *> eval pr is
eval pr (Sub d s:is) = modify (\p -> putDst d p (getDst d p - getSrc s p)) *> eval pr is
eval pr (Cmp a b:is) = modify (\p -> putCmp (compare (getSrc a p) (getSrc b p)) p) *> eval pr is
eval pr (Jeq l:is) = do
  p <- get
  case cmp p of
    EQ -> eval pr $ fromMaybe (lookupErr l) (lookup l pr)
    _  -> eval pr is
eval pr (Jlt l:is) = do
  p <- get
  case cmp p of
    LT -> eval pr $ fromMaybe (lookupErr l) (lookup l pr)
    _  -> eval pr is
eval pr (Jgt l:is) = do
  p <- get
  case cmp p of
    GT -> eval pr $ fromMaybe (lookupErr l) (lookup l pr)
    _  -> eval pr is

-- futtatunk egy nyers programot a startState-ből kiindulva
runProgram :: RawProgram -> ProgState
runProgram rprog = case toProgram rprog of
  []                  -> startState
  prog@((_, start):_) -> execState (eval prog start) startState
