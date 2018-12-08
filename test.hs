-- {-# LANGUAGE RankNTypes #-}

import Control.Monad.ST
import Data.STRef
import Data.IORef
import Data.Array.ST
import System.Random
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Applicative
import MyLib
{-          STRef
fibST :: Int -> ST s Integer
fibST n = do
  a <- newSTRef 0
  b <- newSTRef 1
  repeatFor n
    (do x <- readSTRef a
        y <- readSTRef b
        writeSTRef a y
        writeSTRef b (x+y))
  readSTRef a
  
fib :: Int -> Integer
fib n = runST (fibST n)


repeatFor n = foldr (>>) (return ()) . replicate n
-}




{-         IORef
main :: IO ()
main = do
  ii <- getLine
  let i = (read ii) ::Int
  
  
  a <- newIORef 10

  b <- readIORef a
  writeIORef a (i+b)
  c <- readIORef a
  print c
  
-}
{-           STArray
qqq = runST $ do
  arr <- (newListArray (0, 8) [0..]) :: ST s (STArray s Int Int)
  writeArray arr 0 10
  getElems arr
-}

{-         IOArraay
main :: IO ()
main = do
  arr <- newListArray (0,10) [20..] :: IO (IOArray Int Int)
  aaaa <- getElems arr
  print aaaa
-}

{-
main :: IO ()
main = do
  let arr = ((newListArray (0, 8) [0..]) :: ST s (STArray s Int Int))
  let aaa = runST (arr >>= getElems)
  print aaa
  
-}
{-
main :: IO ()
main = do
  qqq <- return $ runST $ do
    arr <- (newListArray (0, 8) [0..]) :: ST s (STArray s Int Int)
    writeArray arr 0 10
    getElems arr
  print qqq
  
-}
{-
type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
  foldr (>>) (return ()) [push 2,push 10,push 12]
  pop
  pop
  
  
aaa = runState stackManip [5,8,2,1]
-}
{-
pushMS :: Int -> MaybeT (State [Int]) ()
pushMS x = MaybeT $ state $ \xs -> (Just (), x:xs)

pushMS1 :: Int -> MaybeT (State [Int]) ()

pushMS1 x = do
  xs <- lift get
  lift $ put (x:xs)
  lift $ return ()

popMS :: MaybeT (State [Int]) Int
popMS = MaybeT $ state $ \xs -> case xs of
                            [] -> (Nothing, xs)
                            (y:ys) -> (Just y, ys)
popMS1 :: MaybeT (State [Int]) Int
popMS1 = MaybeT $ do
  xs <- get
  case xs of
    [] -> return Nothing
    (y:ys) -> do
      put ys
      return $ Just y
                            
stackMS1 :: MaybeT (State [Int]) Int
stackMS1 = do
  pushMS1 5
  i <- popMS1
  return (i+1)
  
as = (runState $ runMaybeT stackMS1) [1,2,3] -}

{-
data Point = Point {getX :: Int, getY :: Int} deriving (Show)

posXlens :: Functor f => (Int -> f Int) -> (Point -> f Point)
posXlens f (Point x y) = fmap (\x' -> Point x' y) (f x)

point0 = Point 1 6

m = posXlens (Identity . (const 1)) (Point 2 4)
over = posXlens (Identity . (+1)) (Point 2 4)

x = posXlens Const (Point 2 4)


listLens :: Functor f => Int -> (a -> f a) -> ([a] -> f [a])
listLens 0 f (x:xs) = (f x):xs
listLens n f (x:xs) = x:(listLens (n-1) f xs)
-}




{-
ggg :: MaybeT Identity Int
ggg = do 
  MaybeT $ Identity (Just 7)
  MaybeT $ Identity Nothing
  MaybeT $ Identity (Just 8)
  





push :: Int -> MaybeT (State [Int]) ()
push x = MaybeT $ do
  xs <- get
  put (x:xs)
  return $ Just ()
  
pop :: MaybeT (State [Int]) Int
pop = MaybeT $ do
  xss <- get
  case xss of
    [] -> return Nothing
    (x:xs) -> do
      put xs
      return $ Just $ x
      
fff = do
  push 5
  push 6
  pop


-}
{-
g arr = do
  writeArray arr 0 1
  return arr

f = do
  arr <- newArray (0,10) 0 :: ST s (STArray s Int Int)
  a0 <- readArray arr 0
  a1 <- readArray arr 1
  let s=a0+a1
  writeArray arr 0 5
  writeArray arr 1 6
  a0' <- readArray arr 0
  a1' <- readArray arr 1
  let s'=a0'+a1'
  return (s+s')
-}

{-
instance Monad ZipList where
  return = pure
  zipList >>= f = results
    where 
      zipLists = fmap (getZipList . f) zipList   
      results = (!!) <$> zipLists <*> (ZipList [0..])

as = ZipList [1,2,3]
bs = ZipList [4,6,7]

zs = do
  a <- as
  b <- bs
  return (a+b)
-}


a = rmlast [1,2,3,4]














