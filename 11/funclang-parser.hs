{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative
import Control.Monad
import Data.Char

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (g a) s'

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
char c = () <$ satisfy (==c)

anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

string :: String -> Parser ()
string str = mapM_ char str

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep >> pa)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

many_ :: Parser a -> Parser ()
many_ p = () <$ many p

some_ :: Parser a -> Parser ()
some_ p = () <$ some p

type Name = String

data Exp
  = Var Name          -- változó
  | App Exp Exp       -- e1 e2
  | Lam Name Exp      -- \x -> e
  | Let Name Exp Exp  -- let x = e1 in e2
  | Pair Exp Exp      -- (e1, e2)
  | Fst               -- fst
  | Snd               -- snd
  deriving (Eq, Show)

ws :: Parser ()
ws = many_ (char ' ' <|> char '\n')

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

keywords :: [String]
keywords = ["let", "in", "fst", "snd"]

pVar :: Parser Name
pVar = do
  name <- (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
  guard $ notElem name keywords
  ws
  pure name

pAtom :: Parser Exp
pAtom  =  Var <$> pVar
      <|> Fst <$ string' "fst"
      <|> Snd <$ string' "snd"
      <|> (char' '(' *> pExp >>= \e -> Pair e <$> (char' ',' *> pExp <* char' ')')
                                   <|> e <$ char' ')')
      -- <|> Pair <$> (char' '(' *> pExp) <*> (char' ',' *> pExp <* char' ')')
      -- <|> char' '(' *> pExp <* char' ')'

pApp :: Parser Exp
pApp = foldl1 App <$> sepBy1 pAtom (pure ())

pLam :: Parser Exp
pLam = flip (foldr Lam) <$> many (char' '\\' *> pVar <* string' "->") <*> pApp 
-- pLam = Lam <$> (char' '\\' *> pVar) <*> (string' "->" *> (pLam <|> pApp))

pLet :: Parser Exp
pLet = flip (foldr ($)) <$> many (Let <$> (string' "let" *> pVar) <*> (char' '=' *> pExp <* string' "in")) <*> pLam
-- pLet = flip (foldr (\(a, b) -> Let a b)) <$> many ((,) <$> (string' "let" *> pVar) <*> (char' '=' *> pExp <* string' "in")) <*> pLam
-- pLet = Let <$> (string' "let" *> pVar) <*> (char' '=' *> pExp) <*> (string' "in" *> pExp)

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

pExp :: Parser Exp
pExp = pLet

parse :: String -> Maybe (Exp, String)
parse = runParser $ topLevel pExp

main = print $ and
  [
    parse "x" == Just (Var "x",""),
    parse "x1" == Just (Var "x1",""),
    parse "1x" == Nothing,
    parse "f x" == Just (App (Var "f") (Var "x"),""),
    parse "f x y" == Just (App (App (Var "f") (Var "x")) (Var "y"),""),
    parse "f x y z" == Just (App (App (App (Var "f") (Var "x")) (Var "y")) (Var "z"),""),
    parse "f (g x)" == Just (App (Var "f") (App (Var "g") (Var "x")),""),
    parse "fst" == Just (Fst,""),
    parse "snd" == Just (Snd,""),
    parse "f fst snd" == Just (App (App (Var "f") Fst) Snd,""),
    parse "fst (x, y)" == Just (App Fst (Pair (Var "x") (Var "y")),""),
    parse "(x)" == Just (Var "x",""),
    parse "(f x)" == Just (App (Var "f") (Var "x"),""),
    parse "(x, y)" == Just (Pair (Var "x") (Var "y"),""),
    parse "(f x, g y)" == Just (Pair (App (Var "f") (Var "x")) (App (Var "g") (Var "y")),""),
    parse "((x, y))" == Just (Pair (Var "x") (Var "y"),""),
    parse "f (x, y) (z, z)" == Just (App (App (Var "f") (Pair (Var "x") (Var "y"))) (Pair (Var "z") (Var "z")),""),
    parse "\\x -> x" == Just (Lam "x" (Var "x"),""),
    parse "\\x -> f x" == Just (Lam "x" (App (Var "f") (Var "x")),""),
    parse "\\x -> \\y -> x" == Just (Lam "x" (Lam "y" (Var "x")),""),
    parse "\\x -> (x, y)" == Just (Lam "x" (Pair (Var "x") (Var "y")),""),
    parse "(\\x -> x) y" == Just (App (Lam "x" (Var "x")) (Var "y"),""),
    parse "let x = y in x" == Just (Let "x" (Var "y") (Var "x"),""),
    parse "let f = \\x -> x in f" == Just (Let "f" (Lam "x" (Var "x")) (Var "f"),""),
    parse "let f = let x = y in x in f" == Just (Let "f" (Let "x" (Var "y") (Var "x")) (Var "f"),""),
    parse "let foo = x in let bar = y in foo" == Just (Let "foo" (Var "x") (Let "bar" (Var "y") (Var "foo")),"")
  ]