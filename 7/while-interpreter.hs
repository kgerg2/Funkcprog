{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}

import Control.Monad (ap)
import Data.String
import Data.Either
import Data.Maybe
import Data.List

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  State f >>= g = State $ \s -> case f s of
    (a, s') -> runState (g a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= put . f

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

type Name = String

data Exp
  = Add Exp Exp     -- e + e
  | Mul Exp Exp     -- e * e
  | Not Exp         -- not e     (Bool negáció)
  | IntLit Int      -- n
  | BoolLit Bool    -- b
  | Eq Exp Exp      -- e == e   (egyenlőségvizsgálat, Int-re vagy Bool-ra)
  | Lt Exp Exp      -- e < e    (Int rendezés vizsgálata)
  | And Exp Exp     -- e && e   (Bool és)
  | Or Exp Exp      -- e || w   (Bool vagy)
  | Var Name        --          (változónév)
  deriving Show

type Program = [Statement]

infix 3 :=
data Statement
  = Name := Exp              -- értékadás mint infix konstruktor
  | If Exp Program Program
  | While Exp Program
  deriving Show

instance Num Exp where
  (+)         = Add
  (*)         = Mul
  fromInteger = IntLit . fromInteger
  negate e    = Mul e (IntLit (-1))
  abs         = undefined
  signum      = undefined

instance IsString Exp where
  fromString = Var

true :: Exp
true = BoolLit True

false :: Exp
false = BoolLit False

type Val = Either Int Bool
type Env = [(Name, Val)]


tErr :: a
tErr = error "Type error"

evalExp :: Exp -> Env -> Val
evalExp (Add x y)   e = Left  $ fromLeft tErr (evalExp x e) + fromLeft tErr (evalExp y e)
evalExp (Mul x y)   e = Left  $ fromLeft tErr (evalExp x e) * fromLeft tErr (evalExp y e)
evalExp (Not x)     e = Right $ not (fromRight tErr (evalExp x e))
evalExp (IntLit x)  _ = Left  $ x
evalExp (BoolLit x) _ = Right $ x
evalExp (Eq x y)    e = Right $ case evalExp x e of Left  v -> v == fromLeft  tErr (evalExp y e)
                                                    Right v -> v == fromRight tErr (evalExp y e)
evalExp (Lt x y)    e = Right $ fromLeft  tErr (evalExp x e) <  fromLeft  tErr (evalExp y e)
evalExp (And x y)   e = Right $ fromRight tErr (evalExp x e) && fromRight tErr (evalExp y e)
evalExp (Or x y)    e = Right $ fromRight tErr (evalExp x e) || fromRight tErr (evalExp y e)
evalExp (Var x)     e = fromMaybe (error "Undefined variable") (lookup x e)

updEnv :: Name -> Val -> Env -> Env
updEnv x v [] = [(x, v)]
updEnv x v ((x', v'):env)
  | x == x'   = (x', v) : env
  | otherwise = (x', v') : updEnv x v env

evalStatement :: Statement -> State Env ()
evalStatement (x := ex) = do
  modify (\e -> updEnv x (evalExp ex e) e)
-- evalStatement (x := ex) = do
--   e <- get
--   put $ (x, evalExp ex e) : deleteBy ((. fst) . (==) . fst) (x, undefined) e
--   put $ (x, evalExp ex e) : deleteBy (\(a, _) (b, _) -> (a == b)) (x, undefined) e
evalStatement (If ex t f) = do
  e <- get
  if fromRight tErr (evalExp ex e)
    then evalProgram t
    else evalProgram f
evalStatement (While ex p) = do
  e <- get
  if fromRight tErr (evalExp ex e)
    then evalProgram p *> evalStatement (While ex p)
    else pure ()
-- evalStatement s@(While ex p) = evalStatement $ If ex (p ++ [s]) []

evalProgram :: Program -> State Env ()
evalProgram p = mapM_ evalStatement p


-- Program futtatása üres környezetből indulva:
runProgram :: Program -> Env
runProgram prog = execState (evalProgram prog) []

exp1 :: Exp
exp1 = 123 + 432 * 54 -- Add (IntLit 123) (Mul (IntLit 432) (IntLit 54))
-- evalExp exp1 [] == Left 23451

exp2 :: Exp
exp2 = 20 + "x" -- Add (IntLit 20) (Var "x")
-- evalExp exp2 [("x", Left 10)] == Left 30

prog1 :: Program
prog1 = [
  "x"   := 0,
  "acc" := 0,
  While (Not (Eq "x" 20)) [
      "acc" := "acc" + 10,
      "x"   := "x" + 1
      ]
  ]
-- runProgram prog1 == [("x",Left 20),("acc",Left 200)]

prog2 :: Program
prog2 = [
  "b1" := true,
  "x"  := 10,
  If "b1" ["x" := 100] ["x" := 200]
  ]
-- runProgram prog2 == [("b1",Right True),("x",Left 100)]


fact :: Int -> Program
fact n = [
  "in"  := IntLit n,
  "out" := 1,
  While (Lt 0 "in") [
      "out" := "out" * "in",
      "in"  := "in" - 1
      ]
  ]
-- runProgram (fact 5) == [("in",Left 0),("out",Left 120)]
