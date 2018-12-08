import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Cycle
import Control.Monad.State
import Data.Function
import Debug.Trace

data Point = Point Int Int deriving (Eq, Show)
data Line = Line Point Point deriving (Eq, Show)
 
instance Ord Point where
  compare = on compare absPoint 

data Relation = Intersect | Tangential | Away | Contain deriving (Eq, Show)

mapPoint f (Point x y) = Point (f x) (f y)
zipPoint f (Point x0 y0) (Point x1 y1) = Point (f x0 x1) (f y0 y1) 

addPoint = zipPoint (+)
subPoint = zipPoint (-)

absPoint (Point x y) = on (+) abs x y
  

type Shape = Cycle.Cycle Point

makeRect width height = Cycle.fromList [Point 0 0, Point width 0, Point width height, Point 0 height]

lineIntersect :: Line -> Line -> Relation
lineIntersect (Line (Point sx0 sy0) (Point ex0 ey0)) (Line (Point sx1 sy1) (Point ex1 ey1)) = 

move :: Point -> Shape -> Shape
move point shape = fmap (addPoint point) shape



relationPoint :: Point -> Shape -> Relation
relationPoint  
{-
combine :: Shape -> Shape -> Shape
combine shape0 shape1 
-}

