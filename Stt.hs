import System.Random
import Control.Monad.State

newtype Stt s a = Stt { getStt :: s -> (a, s) }

instance Functor (Stt s) where
  fmap = liftM

instance Applicative (Stt s) where
  pure = return
  (<*>) = ap

instance Monad (Stt s) where
  return x = Stt (\s -> (x, s))
  (Stt g0) >>= f0 = Stt g1 
    where { g1 s = f1 s1
              where (a0, s1) = g0 s
                    (Stt f1) = f0 a0}
 
hhh' :: Stt StdGen (Int, Int, Int)
hhh' = do
  a0 <- Stt random
  a1 <- Stt random
  a2 <- Stt random
  return (a0, a1, a2)
  
hhh = fst $ getStt hhh' (mkStdGen 42)
  
  

fff :: (Int, Int, Int)
fff = (a0, a1, a2)
  where (a0, r0) = random $ mkStdGen 42
        (a1, r1) = random r0
        (a2, r2) = random r1


ggg' :: State StdGen (Int, Int, Int)
ggg' = do
  a0 <- state random
  a1 <- state random
  a2 <- state random
  return (a0, a1, a2)
  
ggg = evalState ggg' (mkStdGen 42)


