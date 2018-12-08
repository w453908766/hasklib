import Data.Bits

queen :: Int -> Int -> Int -> Int -> Int -> Int -> Int
queen n row col down left right
 |n==row   = 1
 |col==(shiftL 1 n) = 0
 |otherwise = (queen n row (shiftL col 1) down left right)
            + (if ( (col .&. (down .|. left .|. right)) /= 0) then 0
               else (queen n (row+1) 1 (down .|. col) (shiftL (left .|. col) 1) (shiftR (right .|. col) 1)))
               
Main = print $ queen 16 0 1 0 0 0 
