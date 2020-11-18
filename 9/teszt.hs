{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Bead07 where

import Data.Maybe
import Control.Applicative
import Control.Monad

--------------------------------------------------------------------------------

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> case p s of
    Just (x, s') -> Just (f x, s')
    Nothing      -> Nothing

instance Applicative Parser where pure = return; (<*>) = ap

instance Monad Parser where
  return x = Parser $ \s -> Just (x, s)
  Parser p >>= f = Parser $ \s -> case p s of
    Just (x, s') -> runParser (f x) s'
    Nothing      -> Nothing

--------------------------------------------------------------------------------

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c       -> Just (c, cs)
       | otherwise -> Nothing
  [] -> Nothing

char :: Char -> Parser ()
char c = satisfy (== c) *> pure ()

anyChar :: Parser Char
anyChar = satisfy (const True)

string :: String -> Parser ()
string s = forM_ s char

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

some' :: Alternative f => f a -> f [a]
many' :: Alternative f => f a -> f [a]
some' p = (:) <$> p <*> many p
many' p = some p <|> pure []

--------------------------------------------------------------------------------

-- Bead assignment 07:
--  Define a parser `p :: Parser ()` that parses any number of 'a's, 
--    followed by at least 1 'b', followed by any number of 'c's.
--  The parser p should consume the whole input string.

-- p should correspond to the regular expression     a* b+ c*

p :: Parser ()
p = () <$ many' (char 'a') <* some' (char 'b') <* many' (char 'c') <* eof

-- Examples:

-- p should succeed on any string in this list.
good :: [String]
good = [ "b"
       , "bb"
       , "abc"
       , "aaaaabbbbbbbccccccc"
       , "aaab"
       , "bbbbbc"
       ]

-- p should fail on any string in this list.
bad :: [String]
bad = [ ""
      , "d"
      , "aacc"
      , "a"
      , "cccba"
      , "bbbd"
      , "abcdef"
      ]

--   all isJust    (runParser p) good == True
--   all isNothing (runParser p) bad  == True
