{-# LANGUAGE GADTs, StandaloneDeriving #-}

import Control.Monad
import Control.Applicative
import Data.Functor.Classes
import Data.List as List
import Data.Char as Char
import Data.Map (fromList, Map)
import Data.Tuple
import Text.Printf

data Parser a where
  Pure :: a -> Parser a
  Chr :: Char -> Parser Char
  Or :: Parser a -> Parser a -> Parser a
  Many :: Parser a -> Parser [a]
  Seq :: Parser a -> Parser b -> Parser (a,b)
  Map :: (a->b) -> Parser a -> Parser b

instance Functor Parser where
  fmap = liftA

instance Applicative Parser where
  pure = Pure
  (Pure f) <*> p = Map f p
  (Map f p) <*> q = Map (uncurry f) (Seq p q)

instance Alternative Parser where
  empty = undefined
  (<|>) = Or

instance Show (Parser a) where
  show (Pure x) = "Pure"
  show (Chr x) = (show x)
  show (Or p q) = printf "(%s) <|> (%s)" (show p) (show q)
  show (Many p) = printf "[%s]" (show p)
  show (Seq p q) = printf "%s >> %s" (show p) (show q)
  show (Map f p) = (show p)

parse :: Parser a -> String -> Maybe (a, String)
parse (Chr x) "" = Nothing
parse (Chr x) (c:code)
 |c==x = Just (c, code)
 |otherwise = Nothing

parse (Or p q) code =
  parse p code <|> parse q code

parse (Map f p) code = do
  (x,code') <- parse p code
  return (f x, code')

parse (Seq p q) code = do
  (x,code') <- parse p code
  (y,code'') <- parse q code'
  return ((x,y), code'')

parse (Pure x) code = 
  return (x, code)

parse (Many p) code =
  parse (many p) code

matchStr :: String -> Parser String
matchStr str = traverse Chr str
  

sepBy, sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p q = (:) <$> p <*> Many ps
  where ps = seq <$> q <*> p

sepBy p q = sepBy1 p q <|> Pure []

between :: Parser op -> Parser cl -> Parser a -> Parser a
between open close p = (\a b c->b) <$> open <*> p <*> close

choice :: [Parser a] -> Parser a
choice ps = foldr (<|>) empty ps

------- JSON -------

data Value
 = Boolean Bool
 | StringLit String
 | Array [Value]
 | Object (Map String Value)
 deriving (Show)

bool = Boolean <$> (true <|> false)
  where 
    true = fmap (const True) (matchStr "true")
    false = fmap (const False) (matchStr "false")

string = between (Chr '\'') (Chr '\'') (Many (Chr 'a'))
string_lit = StringLit <$> string

array = Array <$> between (Chr '[') (Chr ']') (sepBy value (Chr ','))

pair = (\a b c->(a,c)) <$> string <*> Chr ':' <*> value
object = (Object . fromList) <$> between (Chr '{') (Chr '}') (sepBy pair (Chr ','))

value :: Parser Value
value = choice [bool, string_lit, array, object]

x = parse value "{'aa':true,'aaa':['aaaa']}"




