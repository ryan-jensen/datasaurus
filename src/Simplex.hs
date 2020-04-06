-- file: src/Simplex.hs

------------------------------------------------------------------------
module Simplex
  ( Simplex
  , valid
  , empty
  , dim
  , asMatrix
  , asVectors
  , translateToO
  , fromList
  , simplexFromList
  , toList
  , qdToSimplex
  , distToSimplex
  , initialPoint
  , points
  , subComplexes
  , faces
  , skeleton
  ) where

------------------------------------------------------------------------
import Numeric.LinearAlgebra           hiding (fromList, fromLists, toList, toLists)
import qualified Numeric.LinearAlgebra as LA
import Point
import qualified Data.Set              as Set
import Data.List
import Data.Ord

------------------------------------------------------------------------
-- Simplex in Euclidean space
data Simplex = Simplex Int [Point] deriving (Show, Read, Eq)

------------------------------------------------------------------------
empty :: Simplex
empty = Simplex (-1) []

dim :: Simplex -> Int
dim (Simplex n _) = n

fromList :: [Point] -> Simplex
fromList ps = Simplex (length ps -1) ps

simplexFromList = fromList

asMatrix :: Simplex -> Matrix Double
asMatrix (Simplex _ rs) = fromRows rs

translateToO :: Simplex -> Simplex
translateToO (Simplex n pts) = Simplex n $ map (\p -> p-(head pts)) pts

asVectors :: Simplex -> Simplex
asVectors s = Simplex n (tail pts)
  where Simplex n pts = translateToO s

valid :: Simplex -> Bool
valid (Simplex (-1) []) = True
valid (Simplex 0 _) = True
valid s = dim s == rank (asMatrix (asVectors s))

toList :: Simplex -> [Point]
toList (Simplex _ pts) = pts

points :: Simplex -> [Point]
points = toList

initialPoint :: Simplex -> Point
initialPoint = head . toList

subComplexes :: Simplex -> [Simplex]
subComplexes s =
  fmap (fromList . Set.toList) $
  (Set.toList . Set.powerSet . Set.fromList . points) s

faces :: Simplex -> [Simplex]
faces = subComplexes

properFaces :: Simplex -> [Simplex]
properFaces s = [s' | s' <- faces s, dim s' >=0, dim s' < dim s]

skeleton :: Int -> Simplex -> [Simplex]
skeleton k s = [s' | s' <- faces s, dim s' >=0, dim s' <= k]

-- ------------------------------------------------------------------------
-- TODO: Clean this up
nearestPointInS' :: Point -> Simplex -> (Point, R)
nearestPointInS' p s =
  minimumBy (comparing snd) $ fmap (nearestPointInS'' p) (faces s)

nearestPointInS'' :: Point -> Simplex -> (Point, R)
nearestPointInS'' p (Simplex (-1) []) = ([1/0,1/0], 1/0)
nearestPointInS'' p (Simplex 0 [q]) = (p, qd p q)
nearestPointInS'' point s =
  -- First translate to the origin
  -- If inside the simplex return the distance from the point to the projection
  if (all (>= 0) alphaL) && (sum alphaL <= 1)
     then (p'+q, qd p p')
     else (p'+q, 1/0)
  where
    -- Translate to origin, put i  as a matrix
    q = initialPoint s
    p = point-q
    a = tr' (asMatrix (asVectors s))
    -- Get coefficients of the projection
    alpha = a <\> p
    alphaL = LA.toList alpha
    -- The projected vector
    p' = a #> alpha

nearestPointInS :: Point -> Simplex -> Point
nearestPointInS p s = fst (nearestPointInS' p s)

qdToSimplex :: Point -> Simplex -> R
qdToSimplex p s = snd (nearestPointInS' p s)

distToSimplex :: Point -> Simplex -> R
distToSimplex p s = sqrt (qdToSimplex p s)

------------------------------------------------------------------------
orig,p0,p1,p2,p3,p4,p5,p6 :: Point
orig = [0,0]
p0   = [8,1]
p1   = [9,5]
p2   = [7,4]
p3   = [5,3]
p4   = [3,1]
p5   = [1,3]
p6   = [3,5]

p7 :: Point
p7 = [1,1]

s0,s1,s2,s3,s4 :: Simplex
s0 = fromList [p0]
s1 = fromList [p1,p2]
s2 = fromList [p2,p3,p6]
s3 = fromList [p3,p5,p6]
s4 = fromList [p3,p4,p5]

s5 :: Simplex
s5 = fromList [p3,p4,p5,p6]

s6 :: Simplex
s6 = fromList [p3,p6]

ep, es0, es1, es2 :: Point
ep  = [4,1]
es0 = [0,0]
es1 = [3,0]
es2 = [2,-1]

esimp = fromList [es0,es1,es2]

t0, t1, t2, p :: Point
t0 = orig
t1 = [1,0]
t2 = [3,1]
p  = [2,-1]

s :: Simplex
s = fromList [t0,t1,t2]
