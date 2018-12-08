import Data.Char
import Data.Tuple
import qualified Data.Map as Map
import SExp hiding (cons, car, cdr)
import qualified SExp (cons) 

data KeyWord = Apply | Begin | Define | If | Lambda | Quote deriving (Show, Eq, Ord)
keyWordMap = Map.fromList [("apply", Apply),
                           ("begin", Begin),
                           ("define", Define),
                           ("if", If),
                           ("lambda", Lambda),
                           ("quote", Quote)]
keyWordMap' = Map.fromList (map swap (Map.toList keyWordMap))

data Var = VarBoolean Bool |
           VarNull |
           VarPair (Var, Var) |
           VarSymbol String |
           VarInt Int | 
           VarChar Char |
           VarString String | 
           VarKeyWord KeyWord
           deriving (Eq, Ord)

instance Show Var where
  show var = show $ varToSExp var
                
cons :: Var -> Var -> Var
cons a d = VarPair (a, d)
car,cdr :: Var -> Var
car (VarPair (a, _)) = a
cdr (VarPair (_, d)) = d

toVar :: String -> Var
toVar "#t" = VarBoolean True
toVar "#T" = VarBoolean True
toVar "#f" = VarBoolean False
toVar "#F" = VarBoolean False
toVar "#\\Space" = VarChar ' '
toVar "#\\Tab" = VarChar '\t'
toVar "#\\Linefeed" = VarChar '\n'
toVar "#\\Return" = VarChar '\r'
toVar ('#':'\\':[x]) = VarChar x
toVar word@(x:_)
 |x=='\"' = VarString (read word)
 |isNumber x = VarInt (read word)
 |Map.member word keyWordMap = VarKeyWord (keyWordMap Map.! word)
 |otherwise = VarSymbol word

sexpToVar :: SExp String -> Var
sexpToVar (Atom x) = toVar x
sexpToVar (SList (Atom "\'":xs)) = cons Quote $ cons (sexpToVar' xs) VarNull
sexpToVar (SList xs) = sexpToVar' xs

sexpToVar' :: [SExp String] -> Var
sexpToVar' [] = VarNull
sexpToVar' [Atom ".", x] = sexpToVar x
sexpToVar' (x:xs) = cons (sexpToVar x) (sexpToVar' xs)

varToSExp :: Var -> SExp String
varToSExp (VarPair (a, d@(VarPair _))) = SExp.cons (varToSExp a) (varToSExp d)
varToSExp (VarPair (a, VarNull)) = SList [varToSExp a]
varToSExp (VarPair (a, d)) = SList [varToSExp a, Atom ".", varToSExp d]
varToSExp VarNull = SList []
varToSExp (VarBoolean True)  = Atom "#t"
varToSExp (VarBoolean False) = Atom "#f"
varToSExp (VarChar ' ') = Atom "#\\Space"
varToSExp (VarChar '\t') = Atom "#\\Tab"
varToSExp (VarChar '\n') = Atom "#\\Linefeed"
varToSExp (VarChar '\r') = Atom "#\\Return"
varToSExp (VarChar x) = Atom ("#\\" ++ [x])
varToSExp (VarSymbol word) = Atom word
varToSExp (VarString word) = Atom (show word)
varToSExp (VarInt num) = Atom (show num)
varToSExp (VarKeyWord key) = Atom (keyWordMap' Map.! key)

eval :: Map.Map String Var -> Var -> Var
eval env (VarPair (op, args)) 


isPair (VarPair _) = True
isAtom = not . isPair

code = "(define a 5)" :: String
lexcode = lexSExp code :: [String]
parsecode = parseSExp lexcode :: SExp String
varcode = sexpToVar parsecode
