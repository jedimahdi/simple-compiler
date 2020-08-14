module Parser where

import Control.Applicative
import Data.Char

newtype Parser a =
  Parser
    { runParser :: String -> Maybe (String, a)
    }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \s -> Just (s, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Monad Parser where
  return = pure
  (>>=) m g =
    Parser
      (\s ->
         case runParser m s of
           Just (s', v) -> runParser (g v) s'
           Nothing -> Nothing)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

anyChar :: Parser Char
anyChar =
  Parser $ \s ->
    case s of
      [] -> Nothing
      (x:xs) -> Just (xs, x)

sat :: (Char -> Bool) -> Parser Char
sat pred = do
  c <- anyChar
  if pred c
    then pure c
    else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

charP :: Char -> Parser Char
charP c = sat (c ==)

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Just (rest, token)

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

fullParsed :: Parser a -> Parser a
fullParsed (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null input'
      then Just (input', xs)
      else Nothing

isParsed :: Parser a -> Parser (a, Bool)
isParsed (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null input'
      then Just (input', (xs, True))
      else Just (input', (xs, False))

ws :: Parser String
ws = spanP isSpace

spaces :: Parser ()
spaces = do
  _ <- many $ charP ' '
  pure ()

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

int :: Parser Int
int = neg <|> nat

nat :: Parser Int
nat = do
  xs <- some digit
  pure $ read xs

neg :: Parser Int
neg = do
  _ <- charP '-'
  negate <$> nat
