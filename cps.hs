newtype Cont r a = Cont { runContT :: (a -> r) -> r } 

show' (Cont c) = c id

instance Functor (Cont r) where
  fmap f c = c >>= (return . f)

instance Applicative (Cont r) where
  pure = return
  mf <*> c = do
    f <- mf
    x <- c
    return (f x)

instance Monad (Cont r) where
  return x = Cont $ \k -> k x
  Cont c >>= f = Cont $ \k -> 
       c $ \a -> runContT (f a) k
       
envPortCPS :: Cont r String
envPortCPS = Cont $ \k -> k "8080"

readIntCPS :: String -> Cont r Int
readIntCPS str = Cont $ \k -> k (read str)

fooCPS :: Cont r Int
fooCPS = do
  port <- envPortCPS
  readIntCPS port
  

