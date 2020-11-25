{-# LANGUAGE DeriveFunctor #-}

import Control.Monad
import Control.Applicative
import Data.Char

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)} deriving Functor

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing      -> Nothing
    Just (a, s') -> runParser (g a) s'

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs -> if f c then Just (c, cs) else Nothing
  []   -> Nothing

anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

char :: Char -> Parser ()
char c = () <$ satisfy (==c)

string :: String -> Parser ()
string str = mapM_ char str

instance Alternative Parser where
  empty = Parser $ \s -> Nothing
  Parser f <|> Parser g = Parser $ \s -> case f s of
    Nothing      -> g s
    Just (a, s') -> Just (a, s')

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

many_ :: Alternative f => f a -> f ()
many_ p = () <$ many p

some_ :: Alternative f => f a -> f ()
some_ p = () <$ some p

data Exp
  = BoolLit Bool       -- true, false
  | StringLit String   -- string literál: idézőjelek között 0 vagy több nem-idézőjel karakter
  | Eq Exp Exp         -- e1 == e2
  | Concat Exp Exp     -- e1 ++ e2
  deriving (Show, Eq)

ws :: Parser ()
ws = many_ (satisfy (\c -> c == ' ' || c == '\n'))

pLit :: Parser Exp
pLit =  ((BoolLit True <$ string "true") <|> (BoolLit False <$ string "false")
    <|> (char '"' *> (StringLit <$> (many (satisfy (/='"')))) <* char '"')) <* ws

pConcat :: Parser Exp
pConcat = do
  xs <- sepBy pLit (ws *> string "++" *> ws)
  return $ foldr1 Concat xs

pEq :: Parser Exp
pEq = Eq <$> pConcat <*> (ws *> string "==" *> ws *> pConcat)

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

pExp :: Parser Exp
pExp = topLevel (pEq <|> pConcat <|> pLit)
