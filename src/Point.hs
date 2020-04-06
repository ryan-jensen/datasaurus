-- file: src/Point.hs

------------------------------------------------------------------------
module Point
  ( Point
  , PointCloud
  , LPointCloud
  , R
  , C
  , I
  , Z
  , quadrance
  , qd
  , norm
  , distance
  , pointFromList
  , pointFromPair
  , pointFromTriple
  , getPointComp
  , getAllComps
  , (!)
  , mpcFromPointCloud
  , numPoints
  , dimPC
  ) where

------------------------------------------------------------------------

import Numeric.LinearAlgebra ( R
                             , C
                             , I
                             , Z
                             , Matrix
                             , Vector
                             , dot
                             , (!)
                             , size
                             , cmap
                             , fromList)

import Numeric.LinearAlgebra.Data ( atIndex
                                  , fromRows
                                  )

------------------------------------------------------------------------
type Point = Vector R

type LPointCloud = [Point]

type PointCloud = Matrix R
--switch back for use with stats?
--type PointCloud = Vector Point

data Point' = Point' Int (Vector R)

------------------------------------------------------------------------
quadrance :: Point -> R
quadrance v = dot v v

qd :: Point -> Point -> R
qd u v = quadrance (u - v)

norm :: Point -> R
norm = sqrt . quadrance

distance :: Point -> Point -> R
distance u v = norm (u - v)

quadrance' :: Point' -> R
quadrance' (Point' _ v) = dot v v

qd' :: Point' -> Point' -> R
qd' (Point' n u) (Point' m v) = quadrance (u-v)

pointFromList :: [R] -> Point
pointFromList = Numeric.LinearAlgebra.fromList

pointFromPair :: (R,R) -> Point
pointFromPair (x,y) = pointFromList [x,y]

pointFromTriple :: (R,R,R) -> Point
pointFromTriple (x,y,z) = pointFromList [x,y,z]

getPointComp :: Point -> Int -> R
getPointComp = (!) --atIndex

getComps :: LPointCloud -> Int -> [R]
getComps pc n = fmap (flip getPointComp n) pc

getAllComps :: LPointCloud -> [[R]]
getAllComps pc = fmap (getComps pc) [0..d]
  where d = size (head pc) - 1

mpcFromPointCloud :: LPointCloud -> PointCloud
mpcFromPointCloud = fromRows

numPoints :: PointCloud -> Int
numPoints pc = r
  where (r,_) = size pc

dimPC :: PointCloud -> Int
dimPC pc = c
  where (_,c) = size pc

p0 = [0,1,2,3,4] :: Point
p1 = [1,2,3,4,5] :: Point

myPC :: LPointCloud
myPC = [p0,p1]
