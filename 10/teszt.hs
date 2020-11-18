{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Bead07 where

import Data.List
import Data.Char
import Data.Maybe
import Control.Applicative hiding (some, many)
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

some :: Alternative f => f a -> f [a]
many :: Alternative f => f a -> f [a]
some p = (:) <$> p <*> many p
many p = some p <|> pure []

ws :: Parser ()
ws = satisfy isSpace *> pure ()

digit :: Parser Integer
digit = fromIntegral . digitToInt <$> satisfy isDigit

posInt :: Parser Integer
posInt = foldl' (\x y -> 10*x+y) 0 <$> some digit

int :: Parser Integer
int = negInt <|> posInt
  where negInt = char '-' *> (negate <$> posInt)

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

--------------------------------------------------------------------------------

-- Bead assignment 08:
--  Write a parser `pList :: Parser [IntExpr]` for comma-separated 
--    lists of integer expressions built from integer constants and +. 
--  The parser should consume the whole input string. 
--
-- Bonus point: also allow paretheses in integer expressions.
-- 
-- You do not have to handle whitespaces.

-- Examples:
--   all (\(s,v) -> runParser pList s == Just (v,"")) good
--   all (\(s,v) -> runParser pList s == Nothing) bad
--   all (\(s,v) -> runParser pList s == Just (v,"")) bonus
good :: [(String, [IntExpr])]
good = [ ("10", [Value 10])
       , ("10+100", [Plus (Value 10) (Value 100)])
       , ("1+2+3+4", [Plus (Plus (Plus (Value 1) (Value 2)) (Value 3)) (Value 4)])
       , ("1,2,3", [Value 1, Value 2, Value 3])
       , ("1+10,2+30,3+60", [Plus (Value 1) (Value 10), Plus (Value 2) (Value 30), Plus (Value 3) (Value 60)])
       ]

bad :: [String]
bad = [ ""
      , "1+"
      , "6+3,"
      , "1+2,+"
      , "1,,3"
      ]

bonus :: [(String, [IntExpr])]
bonus = [ ("((1))", [Value 1])
        , ("1+(2+3)", [Plus (Value 1) (Plus (Value 2) (Value 3))])
        ]

--------------------------------------------------------------------------------

data IntExpr = Value Integer
             | Plus IntExpr IntExpr
             deriving (Eq, Ord, Show)

pIntExpr :: Parser IntExpr
-- pIntExpr = (foldr Plus <$> (Value <$> int <* char '+') <*> (map Value <$> (sepBy int (char '+')))) <|> (Value <$> int)
pIntExpr = do
  xs <- sepBy1 (Value <$> int) (char '+')
  return $ foldl1 Plus xs

pList :: Parser [IntExpr]
-- pList = sepBy pIntExpr (char ',') <* eof
pList = sepBy1 pIntExpr (char ',') <* eof
