-- |

module Parsers where

import           Control.Algebra
import           Control.Applicative
import           Control.Effect.Parser
import           Control.Monad
import           Data.Char
import           Data.Text

data Token = TParens Parens
    | TSym Text
    | TNum Number
    deriving (Eq, Show)

data Number = Decimal Text Text
    | WholeNum Text
    deriving (Eq, Show)

data Parens = LParens
    | RParens
    deriving (Eq, Enum, Show)

lparen :: Has Parser sig m => m Parens
lparen = char '(' >> return LParens

rparen :: Has Parser sig m => m Parens
rparen = char ')' >> return RParens

symbol :: (Alternative m, Has Parser sig m) => m Token
symbol = do
  x  <- passes isAlpha
  xs <- many (passes (\c -> isDigit c || isAlpha c || c == '-'))
  return (TSym (pack (x : xs)))

digit :: (Alternative m, Has Parser sig m) => m Number
digit = do
  nats <- nums
  decs <- option (char '.' *> nums)
  return $ case decs of
    Just x  -> Decimal nats x
    Nothing -> WholeNum nats

nums :: (Alternative m, Has Parser sig m) => m Text
nums = pack <$> some (passes isDigit)

sexp :: (Alternative m, Has Parser sig m) => m [Token]
sexp = some $ do
  skipSpace
  (TParens <$> lparen)
    <|> (TParens <$> rparen)
    <|> (TNum    <$> digit)
    <|> symbol
