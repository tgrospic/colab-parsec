module RhoParser where

import Control.Applicative (many, some, (<|>), liftA2)
import Control.Monad (replicateM, liftM2)
import Data.Functor.Identity (Identity)
import Data.Monoid ((<>))
import Text.Parsec (Parsec, ParseError, anyChar, letter, digit, char, string, parse, try)
import Text.Parsec.Combinator (between, sepBy, sepBy1, choice, chainl1)
import Text.Parsec.Language (haskellDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser, makeTokenParser, natural)

tokenParser :: GenTokenParser String u Identity
tokenParser = makeTokenParser haskellDef

data Proc = SendConst String Integer
          | Nil
          deriving Show

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

nil :: Parser Proc
nil = Nil <$ string "Nil"

nilM :: Parser Proc
nilM = do
  _ <- string "Nil"
  return Nil

-- chan!(42)
send :: Parser Proc
send = SendConst    <$>   sendName     <*>   (exclam *> parens (natural tokenParser))

proc :: Parser Proc
proc = nil <|> send

procM :: Parser Proc
procM = nil <|> send
  where
    send = do
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

r2 = run procM "chan!(42)" -- Right (SendConst "chan" 42)
r3 = run procM "CHAN!(42)" -- Left "(line 1, column 5):\nunexpected \"!\"\nexpecting letter, digit, \"_\", \"'\" or \"#\""
r4 = run procM "CHAN#(42)" -- Right (SendConst "CHAN" 42)

{-
Greg's definition of monad
(M, wrap, roll)

Monad
(M, pure, >>=)

Applicative
(M, pure, <*>)

Functor
(M, <$>)

M = any structure (Maybe, List, Parser, ...)

(<$>) :: Functor f => (a -> b) -> (f a -> f b)
(<$>) :: (a -> b) -> (Parser a -> Parser b)    -- f is the Parser, concrete type
(<$>) = fmap

(<$) :: Functor f => a -> f b -> f a
(<$) a fb = fmap (const a) fb
(<$) a fb = fmap (\_ -> a) fb
 -}

-- prefix (type level) function
plus5 :: (->) Int Int
plus5 = (+5)

-- ghci$ :k (->)
-- (->) :: TYPE q -> TYPE r -> *



{-
Scala example Higher Kinded Type (F[_])
https://github.com/rchain/rchain/blob/87a82925abc45a4f8d95749297b7142c85acc1c6/comm/src/main/scala/coop/rchain/comm/UPnP.scala#L59

This function needs F[_] which implements Log and Monad traits
private def logGatewayEmpty[F[_]: Log: Monad](devices: UPnPDevices): F[Unit] = ...
-}
