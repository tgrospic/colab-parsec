module RhoParser where

import Control.Applicative (many, some, (<|>), liftA2)
import Control.Monad (replicateM, liftM2)
import Data.Functor.Identity (Identity)
import Data.Monoid ((<>))
import Text.Parsec (Parsec, ParseError, anyChar, letter, digit, char, string, parse)
import Text.Parsec.Combinator (between, sepBy, sepBy1, choice, chainl1)
import Text.Parsec.Language (haskellDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser, makeTokenParser, natural)

tokenParser :: GenTokenParser String u Identity
tokenParser = makeTokenParser haskellDef

data Send = SendConst String Integer deriving Show

pvarChar :: Parser Char
pvarChar = letter <|> digit <|> char '_' <|> char '\''

-- token Var (((letter | '\'') (letter | digit | '_' | '\'')*)|(('_') (letter | digit | '_' | '\'')+))
pvar :: Parser String
pvar = (:) <$> (letter <|> char '\'') <*> many pvarChar
  -- dont match the _ character alone
   <|> (:) <$>             char '_'   <*> some pvarChar

sendName :: Parser String
sendName = pvar

exclam :: Parser Char
exclam = char '!'

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- chan!(42)
send :: Parser Send
send = SendConst    <$>   sendName     <*>   (exclam *> parens (natural tokenParser))

sendM :: Parser Send
sendM = do
  chan <- sendName
  _    <- if chan /= "CHAN" then exclam else char '#'
  msg  <- parens (natural tokenParser)
  pure $ SendConst chan msg

run :: Parser a -> String -> Either String a
run p inp = res $ parse p "" inp
  where
  res (Left ex) = Left $ show ex
  res (Right a) = Right a

-- exmples
r1 = run send "chan!(42)" -- Right (SendConst "chan" 42)

r2 = run sendM "chan!(42)" -- Right (SendConst "chan" 42)
r3 = run sendM "CHAN!(42)" -- Left "(line 1, column 5):\nunexpected \"!\"\nexpecting letter, digit, \"_\", \"'\" or \"#\""
r4 = run sendM "CHAN#(42)" -- Right (SendConst "CHAN" 42)
