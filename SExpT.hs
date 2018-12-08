import SExp
import Control.Monad.Trans.Class
import Data.Functor.Classes

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Traversable


newtype SExpT m a = SExpT { runSExpT :: m (SExp a) }

instance (Eq1 m, Eq a) => Eq (SExpT m a) where
    SExpT x == SExpT y = eq1 x y

instance (Ord1 m, Ord a) => Ord (SExpT m a) where
    compare (SExpT x) (SExpT y) = compare1 x y



instance (Eq1 m) => Eq1 (SExpT m) where eq1 = (==)
instance (Ord1 m) => Ord1 (SExpT m) where compare1 = compare

instance (Show1 m) => Show1 (SExpT m) where showsPrec1 = showsPrec

instance (Show1 m, Show a) => Show (SExpT m a) where
    showsPrec d (SExpT m) = showsUnary1 "SExpT" d m


instance (Monad m, Traversable m) => Functor (SExpT m) where
  fmap f sexpt = sexpt >>= (return . f)

instance (Monad m, Traversable m) => Applicative (SExpT m) where
  pure = return
  fs <*> sexpt = do
    f <- fs
    a <- sexpt
    return (f a) 

instance (Monad m, Traversable m) => Monad (SExpT m) where
  return x = SExpT $ return (Atom x)
  (SExpT m) >>= k = SExpT $ do
          sexp <- m
          sequence $ do
            x <- sexp
            sequence $ runSExpT $ k x
--------------
s = SList [Atom 1, Atom 2, SList [Atom 3, SList [Atom 4]], Atom 5]

sexpt = SExpT $ Just $ s
k x = SExpT $ Just $ Atom $ x==5

-- SExpT $  sequence $ do { x<-s; sequence $ runSExpT $ k x}


f :: SExpT Maybe Int
f = do
  a <- lift $ Just 5
  x <- SExpT $ Just s
  return (a+x)
  
g (Left _) = Nothing
g (Right xs) = Just xs

