module Parser where

import Control.Applicative

-- won't work because type class instances must not be type synonyms
-- type Parsera res = String -> (Either String res, String)

-- data Parser res = Parser (String -> (Either String res, String))
-- parse (Parser p) = p
-- or with `newtype`
newtype Parser res = Parser { parse :: String -> (Either String res, String) }

anyChar :: Parser Char
anyChar = Parser nextCh
  where nextCh []     = (Left "End of string", [])
        nextCh (x:xs) = (Right x, xs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = anyChar >>= nextP
  where nextP a =
          if f a
          then pure a
          else Parser (\s -> (Left "", s))

char :: Char -> Parser Char
char c = satisfy (==c)

string :: String -> Parser String
string s = sequence $ char <$> s


-- Monadic API
instance Functor Parser where
  -- pssst using monad to define map
  fmap f fa = fa >>= pure . f

instance Applicative Parser where
  -- pssst using monad to define apply
  fab <*> fa = fab >>= (<$> fa)
  pure a     = Parser (\s -> (Right a, s))

bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser fb afb = Parser p
  where
    p s =
      case parse fb s of
        (Right a, s') -> parse (afb a) s'
        (Left err, s') -> (Left err, s')

instance Monad Parser where
  (>>=)  = bindParser
  return = pure

altParser :: Parser a -> Parser a -> Parser a
altParser a b =  Parser p
  where
    p s =
      case parse a s of
        r@(Right _, _) -> r
        -- when first parser fails we continue with the rest of the input
        -- consumed input from first parser is not visible to second parser
        (Left _, s')    -> parse b s'
        -- we can also parse with the same input as the first parser to implement backtracking
        -- but careful first parser can e.g. `try` the whole input (∞ lookahead)
        -- (Left _, _)     -> parse b s

instance Alternative Parser where
  empty = Parser (\s -> (Left "We don't have empty parser.", s))
  (<|>) = altParser

-- Maybe samples
ma :: Maybe Int
ma = Just 42

plus :: Num a => a -> a -> a
plus a b = a + b

res :: Int
res', res'' :: Maybe Int

res   = plus     42     0
res'  = plus <$> ma <*> Nothing
res'' = (<*>) (fmap plus ma) Nothing


-- Alternative example
abP = char 'a' *> char 'b'
acP = char 'a' *> char 'c'

abcP = abP <|> acP

-- result from first parser
-- parse abcP "ab"   -- (Right 'b',"")

-- second parser can't see consumed 'a' from the first parser
-- parse abcP "ac"   -- (Left "End of string","")
