import qualified Data.Bits as Bits 
import qualified Data.String as String
import qualified Data.Char as Char
import qualified Control.Monad.State as State

data Var = VarInt Int | VarString String | VarList [Var]

instance (Show a) => Show (Var a) where
  show (VarInt x) = show x
  show (VarString str) = str
  show (VarList xs) = show xs

data TokenKind = LeftB | RightB | Atom | Unknown

cons :: Var -> Var -> Var
cons car (VarList cdr) = VarList (car:cdr)

car, cdr :: Var -> Var
car (VarList (x:_)) = x
cdr (VarList (_:xs)) = VarList xs

notAlpha :: Char -> Bool
notAlpha chr = not (Bits.testBit (0x57FFFFFFD7FFFFFFF7FFEC7200000000::Integer) (Char.ord chr))

isLeftB :: Char -> Bool
isLeftB chr = chr `elem` "([{"

isRightB :: Char -> Bool
isRightB chr = chr `elem` ")]}"

lexAtom :: String -> (String, String)
lexAtom = break notAlpha

tokenkind :: String -> TokenKind
tokenkind (chr:_)
  |chr=='(' = LeftB
  |chr==')' = RightB
  |not (notAlpha chr) = Atom
  |otherwise = Unknown

lexSexp :: String -> [String]
lexSexp "" = []
lexSexp code@(chr:last) 
  |Char.isSpace chr = lexSexp last
  |isLeftB chr = "(" : (lexSexp last)
  |isRightB chr = ")" : (lexSexp last)
  |notAlpha chr = ["Unknown Symbol!"]
  |otherwise = token : (lexSexp last')
     where (token, last') = lexAtom code 

parseL :: [String] -> (Var, [String])
parseL [] = (VarList [], [])
parseL tokens@(tok: last) =
  case tokenkind tok of 
    RightB -> (VarList [], tail tokens)
    Unknown -> (VarString "Parse Error!", [])
    _ -> sexp2_tokens2
     where
       (sexp0, tokens0) = parseS tokens 
       (sexp1, tokens1) = parseL tokens0
       sexp2_tokens2 = (cons sexp0 sexp1, tokens1)

parseS :: [String] -> (Var, [String])
parseS [] = (VarList [], [])
parseS (tok: last) =
  case tokenkind tok of
    LeftB -> parseL last
    Atom -> (VarString tok, last)
    RightB -> (VarString "Parse Error!", [])
    Unknown -> (VarString "Parse Error!", [])
    
parseLexp = fst . parseL . lexSexp 




test_code = "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (define (exp a x n) (if (= n 0) a (if (even? n) (exp a (* x x) (/ n 2)) (exp (* a x) x (- n 1)))))"

test_tokens = lexSexp test_code
--test_Sexp = parseLexp 
  
main :: IO ()
main = print test_tokens
