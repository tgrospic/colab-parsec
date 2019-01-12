module Parser where

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
