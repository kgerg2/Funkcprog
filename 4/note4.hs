{-# LANGUAGE DeriveFunctor, MonadComprehensions #-}
module Notes04 where

import Control.Monad (ap)
import Control.Monad.State hiding (liftM, liftM2, liftM3, foldM)
--(State, get, put, runState, execState)

-- Evaluation of expressions
data IntExpr = Value Int
             | Plus  IntExpr IntExpr
             | Times IntExpr IntExpr
             | Div   IntExpr IntExpr

expr1 :: IntExpr
expr1 = Value 10 `Plus` Value 5

expr2 :: IntExpr
expr2 = Value 10 `Times` expr1

expr3 :: IntExpr
expr3 = Value 10 `Div` Value 5

expr4 :: IntExpr
expr4 = Value 10 `Div` Value 0

-- Define `evalIntExpr :: IntExpr -> Int`
-- Examples: 
--   evalIntExpr expr1 == 15
--   evalIntExpr expr2 == 150
--   evalIntExpr expr3 == 2
--   evalIntExpr expr4 == ???

evalIntExpr :: IntExpr -> Int
evalIntExpr (Value x)   = x
evalIntExpr (Plus  a b) = evalIntExpr a   +   evalIntExpr b
evalIntExpr (Times a b) = evalIntExpr a   *   evalIntExpr b
evalIntExpr (Div   a b) = evalIntExpr a `div` evalIntExpr b

-- Define `evalIntExprMaybe :: IntExpr -> Maybe Int`
-- Examples: 
--   evalIntExprMaybe expr1 == Just 15
--   evalIntExprMaybe expr2 == Just 150
--   evalIntExprMaybe expr3 == Just 2
--   evalIntExprMaybe expr4 == Nothing
evalIntExprMaybe :: IntExpr -> Maybe Int
-- evalIntExprMaybe (Div _ (Value 0)) = Nothing
evalIntExprMaybe (Value x)   = Just x
evalIntExprMaybe (Plus  a b) = do
    x <- evalIntExprMaybe a
    y <- evalIntExprMaybe b
    return (x   +   y)
evalIntExprMaybe (Times a b) = do
    x <- evalIntExprMaybe a
    y <- evalIntExprMaybe b
    return (x   *   y)
evalIntExprMaybe (Div   a b) = do
    x <- evalIntExprMaybe a
    y <- evalIntExprMaybe b
    guard (y /= 0)
    return (x `div` y)


-- Some operations on monads
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f a = a >>= return . f

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f a b = a >>= (\a -> liftM (f a) b)

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f a b c = a >>= (\a -> liftM2 (f a) b c)

foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM _ b []     = return b
foldM f b (x:xs) = do
    y <- foldM f b xs
    z <- f y x
    return z

impFactorial :: Integer -> State Integer ()
impFactorial 0 = put 1
impFactorial n = {- do
    x <- impFactorial (n - 1)
    return (x * n) -} undefined
    
runFactorial :: Integer -> Integer
runFactorial n = execState (impFactorial n) 1