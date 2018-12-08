
module MyLib where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.ST
import Control.Monad.State
import Data.Array.ST 
import qualified Data.Array.IArray as IArray
import Debug.Trace



rmlast :: [a] -> ([a], a)
rmlast lst = (init lst, last lst)


on f g x y = f (g x) (g y)  


intCut' 0 _ = return [[]]
intCut' z [] = return []
intCut' z (x:xs)
 |z<x = return []
 |otherwise = do 
   result0 <- intCutMem z xs
   result1 <- intCutMem (z-x) xs
   return $ result0 ++ (map (x:) result1)

intCutMem, intCut' :: Int -> [Int] -> State (Map.Map (Int,[Int]) [[Int]]) [[Int]] 
intCutMem 0 _ = return [[]]
intCutMem _ [] = return []
intCutMem z xs = do
  map0 <- get
  case Map.lookup (z,xs) map0 of
    Just result -> return result
    Nothing -> do
      result <- intCut' z xs
      modify (Map.insert (z,xs) result)
      return result

intCut :: Int -> [Int] -> [[Int]]
intCut z elements' = 
  evalState (intCutMem z elements) Map.empty
  where elements = List.sort elements'
        


consMap :: (Ord a) => a -> b -> Map.Map a [b] -> Map.Map a [b]
consMap key x map0 =
  case Map.lookup key map0 of
    Nothing -> Map.insert key [x] map0
    Just xset -> Map.insert key (x:xset) map0

modifyArray :: (Ix a) => STArray s a b -> a -> (b -> b) -> ST s ()
modifyArray arr i f = do
  x <- readArray arr i
  writeArray arr i (f x)

graftArray :: (Ix a) => STArray s a b -> a -> a -> (b -> b -> b) -> ST s ()
graftArray arr i j f = do
  x <- readArray arr i
  modifyArray arr j (f x)


foldrM :: (Monad m, Foldable t) => (a -> b -> m b) -> b -> t a -> m b
foldrM f acc foldable = foldr (\x -> (>>= (f x))) (return acc) foldable



replaceContainer :: Ord b => ((b->a) -> (fb->fa)) -> ([b] -> [a] -> fb -> fa)
replaceContainer mapFunction bs as container = 
  mapFunction (map0 Map.!) container
  where map0 = Map.fromList (zip bs as)
        

actingOn :: Ord b => ((b->a)->(fb->fa)) -> ([b] -> fb) -> (a -> b) -> [a] -> fa
actingOn mapFunction f key as  =
  replaceContainer mapFunction bs as (f bs)
  where bs = map key as

  

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _ xs [] = xs
mergeBy _ [] ys = ys
mergeBy cmp xss@(x:xs) yss@(y:ys) =
 case cmp x y of
  EQ -> x:(mergeBy cmp xs ys)
  LT -> x:(mergeBy cmp xs yss)
  GT -> y:(mergeBy cmp xss ys)


powerSum' :: STArray s Int [[Int]] -> [Int] -> Int -> ST s [Int]
powerSum' sumArr clue x = do
  let appendAddition i addition = modifyArray sumArr i (++addition)
  additions <- traverse (readArray sumArr) clue  
  sequence $ zipWith appendAddition (map (+x) clue) (map (map (x:)) additions)
  return $ mergeBy compare clue (map (+x) clue)
  

powerSum :: [Int] -> IArray.Array Int [[Int]]
powerSum lst =
 runSTArray $ do
  let lowBound = sum $ filter (<0) lst
  let upBound = sum $ filter (>0) lst
  sumArr <- newArray (lowBound, upBound) [] :: ST s (STArray s Int [[Int]])
  writeArray sumArr 0 [[]]
  foldM (powerSum' sumArr) [0] (reverse lst)
  return sumArr


test z lst = ((powerSum lst) IArray.! z) == (intCut z lst)

sqrtInt :: Int -> Maybe Int
sqrtInt x = List.find ((x==).(^2)) $ map ($ (sqrt $ fromIntegral x)) [ceiling, floor]


filterMaybe :: [Maybe a] -> [a]
filterMaybe [] = []
filterMaybe (Just x:xs) = x:(filterMaybe xs)
filterMaybe (Nothing:xs) = filterMaybe xs


first (a, _, _) = a
second (_, b, _) = b
third (_, _, c) = c

apply1 f [a] = f a
apply2 f [a,b] = f a b
apply3 f [a,b,c] = f a b c
apply4 f [a,b,c,d] = f a b c d


cross2 f [a,b] = [f a b]
cross2 f (a:b:last) = (f a b):(cross2 f (b:last))

cross3 f [a,b,c] = [f a b c]
cross3 f (a:b:c:last) = (f a b c):(cross3 f (b:c:last))

cross4 f [a,b,c,d] = [f a b c d]
cross4 f (a:b:c:d:last) = (f a b c d):(cross4 f (b:c:d:last))

