module SExp (SExp(..), cons, car, cdr, nil, sexpLens, getSExp, setSExp, drain, inject, 
toList, fromList, mapChart, elemIndices, zipSExpWith, getIndexs,
lexSExp, unLexSExp, parseSExp, unParseSExp
) where
import Control.Monad.Identity
import Control.Applicative
import Control.Monad.State
import Control.Monad
import Data.Char
import qualified Data.List as List



data SExp a = Atom a | SList [SExp a] deriving (Eq, Ord, Show)
getAtom (Atom x) = x

{-
data SExp a = Atom {getAtom :: a} | SList {getSList :: [SExp a]} deriving (Eq, Ord)

instance (Show a) => Show (SExp a) where
  show = unLexSExp . unParseSExp . (fmap show)
  -}

instance Monoid (SExp a) where
  mempty = nil
  mappend (SList xs) (SList ys) = SList (xs++ys)

instance Foldable SExp where
  foldr f acc (Atom x) = f x acc
  foldr f acc (SList xs) = foldr (flip (foldr f)) acc xs

instance Traversable SExp where
  traverse f (Atom x) = fmap Atom (f x)
  traverse f (SList xs) = fmap SList $ traverse (traverse f) xs


instance Functor SExp where
  fmap = liftM
  
instance Applicative SExp where
  pure = return
  (<*>) = ap

instance Monad SExp where
  return = Atom
  (Atom x) >>= f = f x
  (SList xs) >>= f = SList (map (>>= f) xs) 


mapChart :: ([SExp a] -> [SExp a]) -> (SExp a -> SExp a)
mapChart f (Atom x) = Atom x
mapChart f (SList xs) = SList $ f $ map (mapChart f) xs


elemIndices x sexp = filter (/=[]) $ foldr (:) [] $ zipSExpWith (\y i->if x==y then i else []) sexp (getIndexs [] sexp)


zipSExpWith :: (a -> b -> c) -> (SExp a -> SExp b -> SExp c)
zipSExpWith f (Atom x) (Atom y) = Atom (f x y)
zipSExpWith f (SList xs) (SList ys) = SList $ zipWith (zipSExpWith f) xs ys


getIndexs :: [Int] -> SExp a -> SExp [Int]
getIndexs index (Atom _) = Atom (reverse index)
getIndexs index (SList xs) = SList $ zipWith (\sexp i->getIndexs (i:index) sexp) xs [0..]


cons :: SExp a -> SExp a -> SExp a
cons car (SList cdr) = SList (car:cdr)

car, cdr :: SExp a -> SExp a
car (SList (x:_)) = x
cdr (SList (_:xs)) = SList xs

nil = SList [] :: SExp a


listLens :: Functor f => Int -> (a -> f a) -> ([a] -> f [a])
listLens 0 f (x:xs) = fmap (:xs) (f x) 
listLens n f (x:xs) = fmap (x:) (listLens (n-1) f xs)

sexpLens :: (Functor f) => [Int] -> (a -> f a) -> ((SExp a) -> f (SExp a))
sexpLens [] f (Atom x) = fmap Atom (f x)
sexpLens (index:indexs) f (SList xs) = fmap SList $ listLens index (sexpLens indexs f) xs

setSExp sexp indexs x = runIdentity $ sexpLens indexs (Identity.(const x)) sexp
getSExp sexp indexs = getConst $ sexpLens indexs Const sexp 

toList :: SExp a -> [a]
toList (SList as) = map getAtom as

fromList :: [a] -> SExp a
fromList xs = SList (map Atom xs)

drain :: SExp a -> (String, [a])
drain = flip drain' ([], [])

drain' :: SExp a -> (String, [a]) -> (String, [a])
drain' (Atom x) (chart, atoms) = ('.':chart, x:atoms)
drain' (SList xs) (chart, atoms) = ('[':chart', atoms')
  where (chart', atoms') = foldr drain' (']':chart, atoms) xs

inject :: (String, [a]) -> SExp a
inject chart_atoms = car $ fst $ runState inject' chart_atoms

inject' :: State (String, [a]) (SExp a)
inject' = do
  chart_atoms <- get
  case chart_atoms of
    ("", []) -> return nil
    ('.':cs, x:xs) -> do
      put (cs, xs)
      sexp <- inject'
      return $ cons (Atom x) sexp
    ('[':cs, xs) -> do
      put (cs, xs)
      sexp0 <- inject'
      sexp1 <- inject'
      return $ cons sexp0 sexp1
    (']':cs, xs) -> do
      put (cs, xs)
      return nil     


isLeftB chr  = elem chr "([{"
isRightB chr = elem chr ")]}"
isDoubleQuotes chr  = chr == '\"'
notAlpha chr = or $ [isSpace, isLeftB, isRightB, isDoubleQuotes] <*> [chr]


breakString :: String -> (String, String)
breakString [] = ("", "")
breakString (x:xs) = (x:str, strLast)
  where (strLast, str) = (breakString' xs)
breakString' ('\'':last) = (last, "\'")
breakString' ('\"':last) = (last, "\"")
breakString' ('\\':x:last) = fmap (('\\':) . (x:)) (breakString' last)
breakString' (x:last) = fmap (x:) (breakString' last)


lexSExp :: String -> [String]
lexSExp "" = []
lexSExp code@(chr:last) 
  |isSpace chr = lexSExp last
  |isLeftB chr = "(" : (lexSExp last)
  |isRightB chr = ")" : (lexSExp last)  
  |otherwise = token : (lexSExp last')
  where (token, last') 
         |isDoubleQuotes chr = breakString code 
         |otherwise   = break notAlpha code


parseS, parseL :: [String] -> (SExp String, [String])
parseS ("(":last) = parseL last
parseS (")":last) = (Atom "Parse Error!", [])
parseS (tok:last) = (Atom tok, last)

parseL [] = (SList [], [])
parseL (")":last) = (SList [], last)
parseL tokens = (cons sexp0 sexp1, tokens1)
  where 
    (sexp0, tokens0) = parseS tokens 
    (sexp1, tokens1) = parseL tokens0

parseSExp :: [String] -> SExp String
parseSExp = fst . parseL


unParseSExp :: SExp String -> [String]
unParseSExp sexp = unParseSExp' sexp []
unParseSExp' (Atom x) last = x:last
unParseSExp' (SList xs) last = "(" : (foldr unParseSExp' (")":last) xs)

unLexSExp :: [String] -> String
unLexSExp [] = ""
unLexSExp ("(":last) = '(':(unLexSExp last)
unLexSExp [x] = x
unLexSExp (x:")":last) = x ++ (unLexSExp (")":last))
unLexSExp (x:last) = x ++ (' ':(unLexSExp last))


code = "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (define (exp a x n) (if (= n 0) a (if (even? n) (exp a (* x x) (/ n 2)) (exp (* a x) x (- n 1)))))"

s = SList [Atom 1, Atom 2, SList [Atom 3, SList [Atom 4]], Atom 5]
a = SList [Atom 2, SList [Atom 3]]
b = SList [Atom 1, SList [Atom 4, Atom 5]]
fa = SList [Atom (*2), SList [Atom (+3)]]

z = fromList [1..3]
y = map (Atom . Just) [1,2]
t = inject ("[..]", [a,b])




