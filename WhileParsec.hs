{-# LANGUAGE GADTs, StandaloneDeriving #-}

import Control.Monad
import Control.Applicative
import Data.Functor.Classes
import Data.List as List
import Data.Char as Char

data Parser a where
  Chr :: Char -> Parser Char
--  Many :: Parser a -> Parser [a]
  Or :: Parser a -> Parser a -> Parser a
  Bind :: Parser b -> (b -> Parser a) -> Parser a
  Pure :: a -> Parser a

instance Functor Parser where
  fmap = liftA

instance Applicative Parser where
  pure = return 
  (<*>) = ap

instance Monad Parser where
  return = Pure
  (>>=) = Bind

instance Alternative Parser where
  empty = undefined
  (<|>) = Or

parse :: Parser a -> String -> Maybe (a, String)
parse (Chr x) "" = Nothing
parse (Chr x) (c:code)
 |c==x = Just (c, code)
 |otherwise = Nothing

parse (Or p q) code =
  parse p code <|> parse q code

parse (Bind p f) code = do
  (x,code') <- parse p code
  parse (f x) code'

parse (Pure x) code = 
  return (x, code)

matchStr :: String -> Parser String
matchStr str = traverse Chr str

------- JSON -------

data Value
 = Boolean Bool
 | StringLit String
 | Array [Value]
 deriving (Show)

bool = do
  let true = matchStr "true" >> return True
  let false = matchStr "false" >> return False
  Boolean <$> (true <|> false)

string_lit = do 
  matchStr "'"
  as <- many (Chr 'a')
  matchStr "'"
  return $ StringLit as

array = do 
  matchStr "["
  v0 <- value
  vs <- many (matchStr "," >> value)
  matchStr "]"
  return $ Array (v0:vs)

value :: Parser Value
value = bool <|> string_lit <|> array


x = parse value "['aaaaa','aaa','aaaaaa']"

