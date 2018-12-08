import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Zipper 
import Control.Monad.State
import Control.Monad.ST
import qualified Data.Array.IArray as IArray
import MyLib
import System.Environment 
import Debug.Trace

data Bottom = Bottom { getTop :: Int, getLength :: Int } deriving (Show, Eq, Ord)
--data Bottom = Bottom Int Int deriving (Show, Eq, Ord)

type Bottoms = Zipper.Zipper Bottom

type Block = Int


bottomCombine :: Bottom -> [Bottom] -> [Bottom]
bottomCombine bottom [] = [bottom]
bottomCombine bottom@(Bottom top0 len0) bottoms@((Bottom top1 len1):rear)
 |top0/=top1 = bottom:bottoms
 |otherwise  = (Bottom top0 (len0+len1)):rear

bottomCover :: [Block] -> ([Block], Bottoms) -> ([Block], Bottoms)
bottomCover newBlocks (blocks, Zipper.Zipper front ((Bottom top _):rear)) =
  (newBlocks++blocks, Zipper.Zipper front' rear')
  where 
    (headBottom : tailBottoms) = [Bottom (top+newSize) newSize|newSize<-newBlocks]
    (headBottom' : front') = bottomCombine headBottom front
    (initBottoms, lastBottom) = rmlast (headBottom':tailBottoms)
    rear' = initBottoms ++ (bottomCombine lastBottom rear)


finishFill :: Int -> ([Block], Bottoms) -> [[Block]] -> [[Block]] 
finishFill rectHeight (blocks, Zipper.Zipper [] [Bottom height _]) continueFill
 |height==rectHeight = [blocks]
 |otherwise = continueFill
finishFill _ _ continueFill = continueFill



searchRect' :: Int -> (IArray.Array Int [[Int]]) -> ([Block], Bottoms) -> [[Block]]
searchRect' rectHeight sumDict (blocks, bottoms) =
  finishFill rectHeight (blocks, bottoms) $ (concat $ map (searchRect' rectHeight sumDict) fillSquare)
  where
    lowBottoms = Zipper.toMinZipperOn id $ Zipper.toList bottoms
    Bottom _ len = Zipper.cursor lowBottoms
    mayPrems = filter (null.(List.intersect blocks)) ((IArray.!) sumDict len)
    mayPrems' = concat $ map List.permutations mayPrems
    fillSquare = map (flip bottomCover (blocks, lowBottoms)) mayPrems'


searchRect :: Int -> Int -> [[Block]]
searchRect width height = 
  map reverse $ concat [searchRect' rectHeight sumDict initState | sumDict<-sumDicts]
  where 
    [rectWidth, rectHeight] = List.sort [width, height]
    sumDicts = map powerSum $ needBlocks rectWidth rectHeight
    initState = ([], Zipper.fromList [Bottom 0 rectWidth])

main = do
  args <- getArgs
  let [rectWidth,rectHeight] = map read args :: [Int]
  print $ searchRect rectWidth rectHeight


needBlocks :: Int -> Int -> [[Int]]
needBlocks rectWidth rectHeight =
  actingOn (map.map) (intCut (rectHeight*rectWidth)) (^2) [1..rectWidth]





{-
hollow :: Bottoms -> Bool
hollow (Zipper.Zipper [] [_]) = True
hollow (Zipper.Zipper [] (Bottom top0 _:Bottom top1 _:_)) = top0<top1
hollow (Zipper.Zipper (Bottom top0 _:_) (Bottom top1 _:Bottom top2 _:_)) = top1<top0 && top1<top2
hollow (Zipper.Zipper (Bottom top0 _:_) [Bottom top1 _]) = top1<top0

searchRect' :: (([Block], Bottoms) -> [[Block]] -> [[Block]]) -> (IArray.Array Int [[Int]]) -> ([Block], Bottoms) -> [[Block]]
searchRect' finishFill' sumDict (blocks, bottoms) =
  finishFill' (blocks, bottoms) $ (concat $ map (searchRect' finishFill' sumDict) fillSquare)
  where
    hollows = filter hollow $ Zipper.enumZipper $ Zipper.toList bottoms 
    shortestBottoms = List.minimumBy (on compare (getLength.(Zipper.cursor))) hollows 
    Bottom _ len = Zipper.cursor shortestBottoms
    mayPrems = filter (null.(List.intersect blocks)) ((IArray.!) sumDict len)
    mayPrems' = concat $ map List.permutations mayPrems
    fillSquare = map (flip bottomCover (blocks, shortestBottoms)) mayPrems'--(trace (show (len,mayPrems')) mayPrems')
-}
