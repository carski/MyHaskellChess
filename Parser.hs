module Parser (Parser, runParser, moveParser) where

import Chess                (Square, Move (..))

import Control.Applicative  (Alternative((<|>), empty))
import Data.List            (elemIndex)


-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser a) = Parser (fmap (first f) . a)
    where
      first :: (a -> b) -> (a,c) -> (b,c)
      first f (x, y) = (f x, y)

instance Applicative Parser where
  pure x = Parser (\xs -> Just (x, xs))
  p1 <*> p2 = Parser func
    where
      func xs =
        case runParser p1 xs of 
          Nothing -> Nothing
          Just (f, as) -> runParser (f <$> p2) as

instance Alternative Parser where
  empty = Parser (const Nothing)
  p1 <|> p2 = Parser (\xs -> runParser p1 xs <|> runParser p2 xs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

-- Read a single char and return its position in the given array if it's in there
charPos :: String -> Parser Int
charPos charArray = Parser f
  where f [] = Nothing
        f (x:xs) = x `elemIndex` charArray >>= \i -> Just (i+1, xs)

squareParser :: Parser Square
squareParser = (,) <$> charPos "abcdefgh" <*> charPos "12345678"

moveParser :: Parser Move
moveParser = Move <$> squareParser <* char ' ' <*> squareParser
