module Zipper 
(Zipper(..), zipperNil, size, fromList, toList, 
uncons, cursor, delete, insert,
moveRight, moveLeft, moveRightist, moveLeftist, moveCount,
right, left, zipperRef,
enumZipper, toMinZipperOn, toMaxZipperOn
) where

import qualified Data.List as List
import Control.Monad

data Zipper a = Zipper { getFront :: [a], getRear :: [a] } deriving (Eq)

instance (Show a) => Show (Zipper a) where
  show (Zipper front rear) = (show $ reverse front) ++ (show rear)

instance (Ord a) => Ord (Zipper a) where
  compare (Zipper front0 rear0) (Zipper front1 rear1) = 
    (compare rear0 rear1 ) `mappend` (on compare reverse front0 front1)

instance Monoid (Zipper a) where
  mempty = Zipper [] []
  mappend (Zipper front rear) zipper1 = Zipper front (rear ++ (toList zipper1))

instance Functor Zipper where
  fmap = liftM
  
instance Applicative Zipper where
  pure x = Zipper [] [x]
--  (Zipper fFront fRear) <*> (Zipper dFront dRear) =
--    Zipper (zipWith ($) fFront dFront) (zipWith ($) fRear dRear) 
  (<*>) = ap
  
instance Monad Zipper where
  return x = Zipper [] [x]
  zipper >>= f =
  (Zipper front rear) >>= f = 
    Zipper (front'++front'') rear'
    where front'' = reverse $ toList $ mconcat $ map f $ reverse front
          Zipper front' rear' = (mconcat $ map f rear)
    
a = moveCount (fromList [1..4]) 2
b = moveCount (fromList [4..7]) 2
y = (+) <$> a <*> b
z = (+) <$> [1..4] <*> [4..7]
---------------------------
zipperNil = Zipper [] []

zipZipperWith f 

on f g x y = f (g x) (g y)  
size (Zipper front rear) = on (+) length front rear
fromList = Zipper []
toList = getRear . moveLeftist
index (Zipper front _) = length front


--------------------
uncons (Zipper front (x:rear)) = (x, Zipper front rear)
cursor = fst . uncons
delete = snd . uncons
insert x (Zipper front rear) = Zipper front (x:rear)
------------------------------

moveRightist (Zipper front rear) = Zipper ((reverse rear) ++ front) []
moveLeftist (Zipper front rear) = Zipper [] ((reverse front) ++ rear)

moveRight (Zipper front (x:rear)) = Zipper (x:front) rear
moveLeft (Zipper (x:front) rear) = Zipper front (x:rear)

right = cursor . moveRight
left = cursor . moveLeft


moveCount zipper i =
  if i>=0 then (iterate moveRight zipper) !! i
  else (iterate moveLeft zipper) !! (-i)


zipperRef zipper i = cursor $ moveCount zipper i
--------------------------------

enumZipper lst = take (length lst) $ iterate moveRight $ fromList lst


toMinZipperOn key lst = List.minimumBy (on compare (key.cursor)) (enumZipper lst)

toMaxZipperOn key lst = List.maximumBy (on compare (key.cursor)) (enumZipper lst)
