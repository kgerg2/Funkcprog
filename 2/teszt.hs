{-# LANGUAGE InstanceSigs #-}
module Bead01 where

-- Bead assignment 01:
--   Define an `Eq` instance for `T`.

data T = SomeInt  Int
       | SomeBool Bool
       deriving (Show)

instance Eq T where
  (==) :: T -> T -> Bool
  SomeInt x  == SomeInt y  = x == y
  SomeBool x == SomeBool y = x == y
  _          == _          = False

-----------
-- Tests --
-----------

s1, s2, s3, s4 :: T
s1 = SomeInt 0
s2 = SomeInt 2
s3 = SomeBool True
s4 = SomeBool False

tests :: [Bool]
tests = [ s1 == s1
        , s2 == s2
        , s3 == s3
        , s4 == s4
        , not $ s1 == s2
        , not $ s2 == s3
        , not $ s3 == s4
        , not $ s4 == s1
        , not $ s1 == s3
        , not $ s4 == s2
        ]

allTests :: Bool
allTests = all (== True) tests

-- `allTests` should be True
