module Bead04 where

import Control.Monad

-- Bead assignment 04:
--   Define the function `evalExpr :: Expr -> Maybe Int`.
--   It should evaluate the input expression in the Maybe monad. 
--   It should fail (and return Nothing) if the expression contains Undefined
--   It should succeed (and return (Just ...)) otherwise.

data Expr = Value Int
          | Undefined
          | Plus Expr Expr

evalExpr :: Expr -> Maybe Int
evalExpr (Value x)  = Just x
evalExpr Undefined  = Nothing
evalExpr (Plus a b) = do
    aa <- evalExpr a
    bb <- evalExpr b
    return (aa + bb)

-- Examples:
--    evalExpr (Value 2 `Plus` Value 3)                    == Just 5
--    evalExpr ((Value 3 `Plus` Value 10) `Plus` Value 3)  == Just 16
--    evalExpr Undefined                                   == Nothing
--    evalExpr (Undefined `Plus` Value 0)                  == Nothing
--    evalExpr ((Value 2 `Plus` Undefined) `Plus` Value 4) == Nothing
