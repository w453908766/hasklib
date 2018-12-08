import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Zipper 
import Control.Monad.State
import Debug.Trace

--data Bottom = Bottom { getTop :: Int, getLength :: Int } deriving (Show, Eq, Ord)
data Bottom = Bottom Int Int deriving (Show, Eq, Ord)

type Bottoms = Zipper.Zipper Bottom

type Block = Int

rectHeight = 32 :: Int
rectWidth = 33 :: Int

bottomCover' :: Int -> Bottom -> [Bottom]
bottomCover' block (Bottom top len) =
  if block+top>rectHeight then []
  else case compare block len of
    GT -> []
    EQ -> [Bottom (top+block) block]
    LT -> [Bottom (top+block) block, Bottom top (len-block)]

bottomCombine :: Bottom -> [Bottom] -> [Bottom]
bottomCombine bottom [] = [bottom]
bottomCombine bottom@(Bottom top0 len0) bottoms@((Bottom top1 len1):rear) =
  if top0/=top1 then bottom:bottoms
  else (Bottom top0 (len0+len1)):rear

bottomCover :: Int -> ([Block], Bottoms) -> ([Block], Bottoms)
bottomCover b blocks_bottoms@(blocks, Zipper.Zipper front (bottom:rear)) =
  case bottomCover' b bottom of
    [] -> blocks_bottoms
    [btm0,btm1] -> (b:blocks, Zipper.Zipper (bottomCombine btm0 front) (bottomCombine btm1 rear))
    [btm] -> (b:blocks, Zipper.Zipper front' rear')
     where (btm':front') = bottomCombine btm front
           rear' = bottomCombine btm' rear

fillSquares :: [Int] -> ([Block], Bottoms) -> [([Block], Bottoms)]
fillSquares [] _ = []
fillSquares (b:bs) blocks_bottoms =
--  trace (show b) $
  if b+top>rectHeight then bottomsList
  else (bottomCover b blocks_bottoms):bottomsList
  where (Bottom top len) = Zipper.cursor $ snd blocks_bottoms
        bottomsList = fillSquares bs blocks_bottoms


searchSquare :: ([Block], Bottoms) -> [([Block], Bottoms)]
searchSquare blocks_bottoms@(blocks, bottoms) =
  if top==rectHeight then [blocks_bottoms]
  else concat $ map searchSquare blockInc
  where lowBottoms = Zipper.moveToMin bottoms
        (Bottom top len) = Zipper.cursor lowBottoms   
        blockInc = fillSquares ((List.\\) [1..len] blocks) blocks_bottoms 

s0 = ([], Zipper.fromList [Bottom 0 rectWidth])
sa = bottomCover 33 s0
sb = bottomCover 37 sa
sc = bottomCover 42 sb
sde = bottomCover 29 $ fmap Zipper.moveLeftist sc
sf = bottomCover 4 sde

main = do
  print $ searchSquare s0

