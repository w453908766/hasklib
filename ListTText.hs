import Control.Monad.List

f :: ListT Maybe Int
f = do
  lift $ Just 4
  i <- ListT $ Just [1,2,3]
  j <- ListT $ Nothing
  return $ i+j
