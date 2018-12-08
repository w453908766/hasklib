import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Cycle
import Control.Monad.State
import Data.Function
import Debug.Trace

data Point = Point Int Int deriving (Eq, Ord, Show)
--data Line = Line Point Point deriving (Eq, Ord, Show)
--data Angle = Angle Point Point Point deriving (Eq, Ord, Show)
type Area = Cycle.Cycle Point 

type Block = Int

xLens, yLens :: (Functor f) => (Int -> f Int) -> (Point -> f Point)
xLens f (Point x y) = fmap (flip Point y) (f x)
yLens f (Point x y) = fmap (Point x) (f y)

distance (Point x0 y0) (Point x1 y1) = on (+) abs (x1-x0) (y1-y0)
  

hollow (Point x0 y0) (Point x1 y1) (Point x2 y2) (Point x3 y3) =
  if x1==x2 then hollow' (y2-y1) x0 x1 x3
  else hollow' (x2-x1) y0 y1 y3
  where hollow' diff c0 c12 c3 = 
          if (c3-c12)*(c12-c0)>0 then maxBound else abs diff

hollows cycle = Cycle.cross4 hollow $ Cycle.moveLeft cycle

findMinHollow :: Area -> Area
findMinHollow Area = List.minimumBy (on compare hollows) (Cycle.rotate Area) 


bottomCover :: [Block] -> ([Block], Area) -> ([Block], Area)
bottomCover [newBlock] (blocks, Cycle.Cycle front (Point x0 y0:Point x1 y1:rear)) =
  (newBlock:blocks, Cycle.Cycle front' rear')
  where 


searchRect :: [Block] -> ([Block], Area) -> [([Block], Area)]
searchRect modBlocks (blocks, ares) = 
  concat $ map (searchRect modBlocks) fillSquare
  where minHollow = findMinHollow ares
        minLen = distance (Cycle.cursor minHollow) (Cycle.right minHollow)
        mayPerms = separate minLen blocks
        fillSquare = map (flip bottomCover (blocks, minHollow)) mayPerms

rectHeight = 32 :: Int
rectWidth = 33 :: Int
{-
initBlocks = [1,4,7,8,9,10,14,15,18] :: [Block]
initArea = Cycle.fromList [Point 0 0, Point 33 0, Point 33 32, Point 0 32]
-}
initBlocks = [1,4,7,8,9,10,14,15] :: [Block]
initArea = Cycle.fromList [Point 33 0, Point 33 32, Point 0 32, Point 0 18, Point 18 18, Point 18 0]
                           



separate z xs = concat $ map List.permutations $ List.sortBy (compare `on` length) $ separate' z [] xs
separate' 0 zs _ = [zs]
separate' _ _ [] = []
separate' z zs (x:xs) = 
  if z<x then separate' z zs xs
  else (separate' z zs xs) ++ (separate' (z-x) (x:zs) xs)
