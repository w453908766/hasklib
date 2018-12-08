module Cycle
(Cycle(..), ret, (>>>=), liftCycle,
size, toList, fromList, toCycle, 
uncons, cursor, delete, insert, 
moveRight, moveLeft, right, left, moveCount, cycleRef, 
cross, cross3, cross4, rotate, moveToMin) where 

import Data.Function
import qualified Data.List as List
import Control.Monad

data Cycle a = Cycle { getFront :: [a], getRear :: [a] }

instance (Show a) => Show (Cycle a) where
  show cycle = "Cycle" ++ (show $ toList cycle)
  
instance (Eq a) => Eq (Cycle a) where
  (==) = on (==) toList

instance (Ord a) => Ord (Cycle a) where
  compare = on compare toList
  

instance Monoid (Cycle a) where
  mempty = Cycle [] []
  mappend = liftCycle (++)

instance Foldable Cycle where
  foldr f acc cycle = foldr f acc (toList cycle)

instance Traversable Cycle where
  traverse f cycle = fmap fromList (traverse f (toList cycle))

instance Functor Cycle where
  fmap = liftM
  
instance Applicative Cycle where
  pure = return
  (<*>) = ap
  
instance Monad Cycle where
  return x = fromList [x]
  cycle >>= f = mconcat $ map f $ toList cycle
  
  
ret = fromList
(>>>=) :: Cycle a -> ([a] -> Cycle b) -> Cycle b
cycle >>>= f = f (toList cycle)

liftCycle :: ([a] -> [b] -> [c]) -> (Cycle a -> Cycle b -> Cycle c)
liftCycle f cycle0 cycle1 = fromList $ f (toList cycle0) (toList cycle1)


zipCycleWith f = on (zipWith f) toList

size (Cycle front rear) =  on (+) length front rear
toList (Cycle front rear) = rear ++ (reverse front)
fromList = Cycle []

toCycle n lst = Cycle (reverse rear) front
  where (front, rear) = splitAt n lst



-----------------------------
uncons (Cycle front (x:rear)) = (x, Cycle front rear)
cursor = fst . uncons
delete = snd . uncons

insert x (Cycle front rear) = Cycle front (x:rear)
---------------------------------

splitHalf lst = splitAt (div (length lst) 2) lst


moveRight (Cycle front [x]) = 
  Cycle front' (reverse rear')
  where (front', rear') = splitHalf (x:front)      
moveRight (Cycle front (x:rear)) = Cycle (x:front) rear

     
moveLeft (Cycle [] rear) =
  Cycle (reverse $ init rear') (last rear':front')
  where (front', rear') = splitHalf rear 
moveLeft (Cycle (x:front) rear) = Cycle front (x:rear)
        
right = cursor . moveRight
left = cursor . moveLeft

moveCount cycle i =
  if i>=0 then (iterate moveRight cycle) !! i
  else (iterate moveLeft cycle) !! (-i)

cycleRef cycle i = cursor $ moveCount cycle i
---------------------------

cas = fromList [1..7]
cbs = fromList [5..11]
czs = cas >>>= \as -> cbs >>>= \bs -> ret (zipWith (+) as bs)

cross f cycle = f <$> cycle <*> (moveRight cycle)
cross3 f cycle = f <$> (cycles!!0) <*> (cycles!!1) <*> (cycles!!2)
  where cycles = rotate cycle
cross4 f cycle = f <$> (cycles!!0) <*> (cycles!!1) <*> (cycles!!2) <*> (cycles!!3)
  where cycles = rotate cycle
    

  
rotate cycle = take (size cycle) $ iterate moveRight cycle

moveToMin cycle = List.minimumBy (on compare cursor) (rotate cycle)

