{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MonadComprehensions #-}
module Notes08 where

import Data.Char
import Control.Applicative (Alternative(..))
import Control.Monad (ap)

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

-- The `eof` (end of file) parser.
--  succeeds if the input string is empty, fails otherwise.
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- The parser `satisfy p` succeeds if the input string starts with a character 
--  that satisfies the predicate p (and consumes that character), and fails otherwise.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c       -> Just (c, cs)
       | otherwise -> Nothing
  [] -> Nothing

-------------------------------------------------------------------------------

-- The parser `char c` should succeed if the input string start with the character c.
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- The parser anyChar should succeed if the input string is not empty, and return its first character.
anyChar :: Parser Char
anyChar = satisfy (const True)

-- The parser `string s` should succeed if the input string starts with the string s.
string :: String -> Parser ()
string [] = Parser (\s -> Just ((), s))
string (c:cs) = char c *> string cs

-------------------------------------------------------------------------------

instance Alternative Parser where
  -- The parser `empty` always fails.
  empty = Parser $ \_ -> Nothing

  -- The parser `p1 <|> p2` tries the parser p1. 
  --  If p1 succeeds, it returns the result of p1.
  --  If p1 fails, then it tries the parser p2 instead.
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-------------------------------------------------------------------------------

-- The parser `some p` and `many p` both try to use the parser p as many times as possible.
--  `many p` always succeeds.
--  `some p` succeeds if the first run of p succeeded.

some' :: Alternative f => f a -> f [a]
many' :: Alternative f => f a -> f [a]
some' p = (:) <$> p <*> many' p
many' p = some' p <|> pure []

-- Examples:
--   runParser (some (char 'a')) "aaabbb" = Just ("aaa", "bbb")
--   runParser (some (char 'a')) "bbb" = Nothing
--   runParser (many (char 'a')) "aaabbb" = Just ("aaa", "bbb")
--   runParser (many (char 'a')) "bbb" = Just ("", "bbb")


-------------------------------------------------------------------------------

-- The parser digit should parse a digit between 0 and 9.
digit :: Parser Integer
-- digit = fmap (\c -> fromIntegral $ ord c - ord '0') $ satisfy (\c -> c >= '0' && c <= '9')
digit = fmap (fromIntegral . digitToInt) $ satisfy (\c -> c >= '0' && c <= '9')

-- The parser digit should parse a positive integer
posInt :: Parser Integer
posInt = fmap (foldl (\x n -> x * 10 + n) 0) $ (char '+' *> some' digit) <|> some' digit

-- The parser int should parse a positive or negative integer
int :: Parser Integer
int = posInt <|> char '-' *> fmap ((-1)*) posInt

-- The parser `space` should parse a single whitespace character.
--  Hint: use `isSpace :: Char -> Bool`
space :: Parser ()
space = () <$ satisfy isSpace

-- The parser `ws` should parse as many whitespace characters as possible.
ws :: Parser ()
ws = () <$ many space

--------------------------------------------------------------------------------
-- Parsing a simple configuration file.
-- A configuration file is a list of lines "key = value" where key is an 
--  identifier and value is an integer.

pLine :: Parser (String, Integer)
-- pLine = (,) <$> (ws *> some' (satisfy (not . isSpace))) <*> (ws *> char '=' *> int <* ws)
pLine = do
  ws
  s <- some' (satisfy (not . isSpace))
  ws
  char '='
  ws
  n <- int
  ws
  pure (s, n)

pFile :: Parser [(String, Integer)]
pFile = many pLine

test1, test2, test3, test4 :: String
test1 = ""
test2 = "key = 10"
test3 = "key1 = 10 \nkey2 = 100"
test4 = "  key   =   10   \n   key2  =   0   \n   key3  =  -10 "

--------------------------------------------------------------------------------
