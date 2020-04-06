module Datasaurus where

import Point as P
import Simplex as S
import SimplicialComplex as SC
import SimplicialComplexShapes as Shape
import Numeric.LinearAlgebra as M
import Numeric.LinearAlgebra.Data as M
import qualified Numeric.GSL.Statistics as S
import Control.Monad.Trans.State
import System.Random

type Bound = (R,R)
type Bounds = [Bound]

data PointCloudState = PCS { pointCloud    :: PointCloud
                           , generator     :: StdGen
                           , targetComplex :: SimplicialComplex
                           , meanBounds    :: Bounds
                           , stddevBounds  :: Bounds
                           , movementBound :: Bound
                           , index         :: Int
                           } deriving (Show)

-- more general bounds?
myPCS = PCS { pointCloud    = test0
            , generator     = mkStdGen 0
            , targetComplex = mySC
            , meanBounds    = [(-1,2), (-1,2)]
            , stddevBounds  = [(-1,2), (-1,2)] -- add a make bounds functions
            , movementBound = (-0.1,0.1)
            , index         = 0
            }

datasPCS = PCS { pointCloud = datasaurus
               , generator  = mkStdGen 0
               , targetComplex = hLines2
               , meanBounds    = [(54.0, 55.0), (47.0,48.0)]
               , stddevBounds  = [(16.0, 17.0), (26.0,27.0)]
               , movementBound = (-0.5,0.5)
               , index         = 0
               }

mkPointCloud tc = PCS { pointCloud = datasaurus
                      , generator = mkStdGen 0
                      , targetComplex = tc
                      , meanBounds = [(54.0,55.0), (47.0, 48.0)]
                      , stddevBounds = [(16.0,17.0), (26.0, 27.0)]
                      , movementBound = (-0.5,0.5)
                      , index         = 0
                      }

mkPointCloud' ::
  PointCloud -> SimplicialComplex -> Int -> PointCloudState
mkPointCloud' ipc tc sgn = PCS { pointCloud = ipc
                               , generator = mkStdGen sgn
                               , targetComplex = tc
                               , meanBounds = [(54.0,55.0), (47.0, 48.0)]
                               , stddevBounds = [(16.0,17.0), (26.0, 27.0)]
                               , movementBound = (-0.5,0.5)
                               , index         = 0
                               }



newPoint :: State PointCloudState (Int, Point)
newPoint = do
  currentState <- get
  let (pc, gen, target, ix) = ( pointCloud currentState
                            , generator currentState
                            , targetComplex currentState
                            , index currentState
                            )
  let mvBnd = movementBound currentState
  let (rowNum, gen') = randomR (1,numPoints pc) gen
  let (newSeed, gen'') = next gen'
  let (temp, gen''') = randomR (0,1::Double) gen''
  let newRow' = uniformSample newSeed 1 $ replicate (dimPC pc) mvBnd
  let oldRow = pc ! (rowNum -1)
  let newRow = oldRow + (flatten newRow')
  put currentState {generator = gen''', index = ix +1}
  case (qdToSC newRow target <= qdToSC oldRow target)
       || (temp < cooling (fromIntegral ix)) of
    True -> do
      return (rowNum, newRow)
    False -> do
      newPoint

sndPC :: PointCloud
sndPC = (iters datasPCS) !! 1750

cooling :: R -> R
cooling x = a*x^2 +b
  where
    maxIters = 100000
    a' = 0.01
    b = 0.4
    a = (a'-b)/maxIters^^2

iters :: PointCloudState -> [PointCloud]
iters = evalState (sequence $ repeat nextIter)

totalQd :: PointCloud -> SimplicialComplex -> R
totalQd pc sc = sum (map (flip qdToSC sc) (toRows pc))

nextIter :: State PointCloudState PointCloud
nextIter = do
  (rowNum, newRow) <- newPoint
  currentState <- get
  let
    currentPC = pointCloud currentState
    newPC = swapRow currentPC rowNum newRow
    newState = currentState { pointCloud = newPC }
  case (withInBounds newState) of
    True -> do
      put newState
      return newPC
    False -> do
      nextIter

withInBounds :: PointCloudState -> Bool
withInBounds pcs = (all withInBound tmp) && (all withInBound tmp2)
  where
    pc = pointCloud pcs
    pcMeans = means pc
    pcSD = sqrt (variances pc)
    meanBnds =   meanBounds pcs
    stddevBnds = stddevBounds pcs
    tmp = zip (M.toList pcMeans) meanBnds
    tmp2 = zip (M.toList pcSD) stddevBnds

withInBound :: (R, Bound) -> Bool
withInBound (r, (l,h)) = (l<r) && (r<h)

swapRow :: PointCloud -> Int -> Point -> PointCloud
swapRow pc rowNum newPoint = (takeRows (rowNum -1) pc)
                             ===
                             asRow newPoint
                             ===
                             (dropRows rowNum pc)

getMeans :: PointCloud -> [R]
getMeans mptc = fmap S.mean (M.toColumns mptc)

getSTDDEVs :: PointCloud -> [R]
getSTDDEVs mptc = fmap S.stddev (M.toColumns mptc)

getSkews :: PointCloud -> [R]
getSkews mptc = fmap S.skew (M.toColumns mptc)

getVariances :: PointCloud -> [R]
getVariances mptc = fmap S.variance (M.toColumns mptc)

means :: PointCloud -> Point
means = fst . meanCov

coVarMatrix :: PointCloud -> Matrix R
coVarMatrix = unSym . snd . meanCov

-- does a point make sense here?
variances :: PointCloud -> M.Vector R
variances = M.takeDiag . coVarMatrix

test0 :: PointCloud
test0 = mpcFromPointCloud $ fmap pointFromList test0'
test0' = [ [1,2]
         , [1.5,1.5]
         , [3,1.75]
         , [1,0.5]
         ]

vLines2, vLines4 :: SimplicialComplex
vLines2 = mkSimplicialComplex 1 [vLine0, vLine1]
vLines4 = mkSimplicialComplex 1 [vLine0, vLine1, vLine2, vLine3]

vLine0 = simplexFromList [[30,0], [30,100]]
vLine1 = simplexFromList [[70,0], [70,100]]
vLine2 = simplexFromList [[50,0], [50,100]]
vLine3 = simplexFromList [[90,0], [90,100]]

hLines2, hLines4 :: SimplicialComplex
hLines2 = mkSimplicialComplex 1 [hLine0, hLine1]
hLines4 = mkSimplicialComplex 1 [hLine0, hLine1, hLine2, hLine3]

hLine0 = simplexFromList [[0,10], [100,10]]
hLine1 = simplexFromList [[0,90], [100,90]]
hLine2 = simplexFromList [[0,37], [100,37]]
hLine3 = simplexFromList [[0,64], [100,64]]

sqr4 :: SimplicialComplex
sqr4 = mkSimplicialComplex 1 ([s0, s1] ++ ss)
  where
    (_, ss) = unSimplicialComplex sqr
    s0 = simplexFromList [[55,18], [55,78]]
    s1 = simplexFromList [[25,48], [85,48]]

wedge :: SimplicialComplex
wedge = mkSimplicialComplex 1 (c0 ++ c1)
  where
    r = 22
    xCenter = 54.26
    yCenter = 47.83
    c0 = circ' r (xCenter, yCenter + r)
    c1 = circ' r (xCenter, yCenter - r)

wedge4 :: SimplicialComplex
wedge4 = mkSimplicialComplex 1 (c0 ++ c1 ++ c2 ++ c3)
  where
    r = 16
    xCenter = 54.26
    yCenter = 47.83
    c0 = circ' r (xCenter - r, yCenter - r)
    c1 = circ' r (xCenter + r, yCenter - r)
    c2 = circ' r (xCenter - r, yCenter + r)
    c3 = circ' r (xCenter + r, yCenter + r)

gridShape :: SimplicialComplex
gridShape = mkSimplicialComplex 0 ps
  where ps = fmap simplexFromList [ [[20,15]], [[50,15]], [[80,15]]
                                  , [[20,50]], [[50,50]], [[80,50]]
                                  , [[20,85]], [[50,85]], [[80,85]]
                                  ]

-- tmpPs :: [Simplex]
-- tmpPs = fmap simplexFromList [ [[20,15]], [[50,15]], [[80,85]]
--                              , [[20,50]], [[50,50]], [[80,50]]
--                              , [[20,85]], [[50,85]], [[80,85]]
--                              ]

s1 :: SimplicialComplex
s1 = mkSimplicialComplex 1 (circ 32 32)

circ :: Double -> Double -> [Simplex]
circ rx ry = fromPairs $ circ'' rx ry (54.26, 47.83) 40

circ' :: Double -> (Double, Double) -> [Simplex]
circ' r (h,k) = fromPairs $ circ'' r r (h,k) 40

circ'' rx ry (h,k) npts = [(h+rx*cos t, k+ry*sin t) | t <- [0,stp..2*pi :: Double]]
  where stp = 2*pi/npts

fromPairs :: [(Double, Double)] -> [Simplex]
fromPairs [] = []
fromPairs [(x,y)] = []
fromPairs ((x1,y1):(x2,y2):xs) = seg:fromPairs ((x2,y2):xs)
  where seg = simplexFromList [[x1,y1], [x2,y2]]
--fromPairs ps = M.fromLists $ map (\(i,j) -> [i,j]) ps

sqr :: SimplicialComplex
sqr = mkSimplicialComplex 1 [s0,s1,s2,s3]
  where
    s0 = simplexFromList [[85,18], [85,78]]
    s1 = simplexFromList [[25,18], [25,78]]
    s2 = simplexFromList [[25,18], [85,18]]
    s3 = simplexFromList [[25,78], [85,78]]

xShape :: SimplicialComplex
xShape = mkSimplicialComplex 1 [d0, d1]
  where
    d0 = simplexFromList [[20,0], [100,100]]
    d1 = simplexFromList [[20,100], [100,0]]

datasaurus :: PointCloud
datasaurus = mpcFromPointCloud $ fmap pointFromPair datasaurusRaw

datasaurusRaw :: [(R,R)]
datasaurusRaw = [ (55.3846, 97.1795)
                , (51.5385, 96.0256)
                , (46.1538, 94.4872)
                , (42.8205, 91.4103)
                , (40.7692, 88.3333)
                , (38.7179, 84.8718)
                , (35.641, 79.8718)
                , (33.0769, 77.5641)
                , (28.9744, 74.4872)
                , (26.1538, 71.4103)
                , (23.0769, 66.4103)
                , (22.3077, 61.7949)
                , (22.3077, 57.1795)
                , (23.3333, 52.9487)
                , (25.8974, 51.0256)
                , (29.4872, 51.0256)
                , (32.8205, 51.0256)
                , (35.3846, 51.4103)
                , (40.2564, 51.4103)
                , (44.1026, 52.9487)
                , (46.6667, 54.1026)
                , (50, 55.2564)
                , (53.0769, 55.641)
                , (56.6667, 56.0256)
                , (59.2308, 57.9487)
                , (61.2821, 62.1795)
                , (61.5385, 66.4103)
                , (61.7949, 69.1026)
                , (57.4359, 55.2564)
                , (54.8718, 49.8718)
                , (52.5641, 46.0256)
                , (48.2051, 38.3333)
                , (49.4872, 42.1795)
                , (51.0256, 44.1026)
                , (45.3846, 36.4103)
                , (42.8205, 32.5641)
                , (38.7179, 31.4103)
                , (35.1282, 30.2564)
                , (32.5641, 32.1795)
                , (30, 36.7949)
                , (33.5897, 41.4103)
                , (36.6667, 45.641)
                , (38.2051, 49.1026)
                , (29.7436, 36.0256)
                , (29.7436, 32.1795)
                , (30, 29.1026)
                , (32.0513, 26.7949)
                , (35.8974, 25.2564)
                , (41.0256, 25.2564)
                , (44.1026, 25.641)
                , (47.1795, 28.718)
                , (49.4872, 31.4103)
                , (51.5385, 34.8718)
                , (53.5897, 37.5641)
                , (55.1282, 40.641)
                , (56.6667, 42.1795)
                , (59.2308, 44.4872)
                , (62.3077, 46.0256)
                , (64.8718, 46.7949)
                , (67.9487, 47.9487)
                , (70.5128, 53.718)
                , (71.5385, 60.641)
                , (71.5385, 64.4872)
                , (69.4872, 69.4872)
                , (46.9231, 79.8718)
                , (48.2051, 84.1026)
                , (50, 85.2564)
                , (53.0769, 85.2564)
                , (55.3846, 86.0256)
                , (56.6667, 86.0256)
                , (56.1538, 82.9487)
                , (53.8462, 80.641)
                , (51.2821, 78.718)
                , (50, 78.718)
                , (47.9487, 77.5641)
                , (29.7436, 59.8718)
                , (29.7436, 62.1795)
                , (31.2821, 62.5641)
                , (57.9487, 99.4872)
                , (61.7949, 99.1026)
                , (64.8718, 97.5641)
                , (68.4615, 94.1026)
                , (70.7692, 91.0256)
                , (72.0513, 86.4103)
                , (73.8462, 83.3333)
                , (75.1282, 79.1026)
                , (76.6667, 75.2564)
                , (77.6923, 71.4103)
                , (79.7436, 66.7949)
                , (81.7949, 60.2564)
                , (83.3333, 55.2564)
                , (85.1282, 51.4103)
                , (86.4103, 47.5641)
                , (87.9487, 46.0256)
                , (89.4872, 42.5641)
                , (93.3333, 39.8718)
                , (95.3846, 36.7949)
                , (98.2051, 33.718)
                , (56.6667, 40.641)
                , (59.2308, 38.3333)
                , (60.7692, 33.718)
                , (63.0769, 29.1026)
                , (64.1026, 25.2564)
                , (64.359, 24.1026)
                , (74.359, 22.9487)
                , (71.2821, 22.9487)
                , (67.9487, 22.1795)
                , (65.8974, 20.2564)
                , (63.0769, 19.1026)
                , (61.2821, 19.1026)
                , (58.7179, 18.3333)
                , (55.1282, 18.3333)
                , (52.3077, 18.3333)
                , (49.7436, 17.5641)
                , (47.4359, 16.0256)
                , (44.8718, 13.718)
                , (48.7179, 14.8718)
                , (51.2821, 14.8718)
                , (54.1026, 14.8718)
                , (56.1538, 14.1026)
                , (52.0513, 12.5641)
                , (48.7179, 11.0256)
                , (47.1795, 9.8718)
                , (46.1538, 6.0256)
                , (50.5128, 9.4872)
                , (53.8462, 10.2564)
                , (57.4359, 10.2564)
                , (60, 10.641)
                , (64.1026, 10.641)
                , (66.9231, 10.641)
                , (71.2821, 10.641)
                , (74.359, 10.641)
                , (78.2051, 10.641)
                , (67.9487, 8.718)
                , (68.4615, 5.2564)
                , (68.2051, 2.9487)
                , (37.6923, 25.7692)
                , (39.4872, 25.3846)
                , (91.2821, 41.5385)
                , (50, 95.7692)
                , (47.9487, 95)
                , (44.1026, 92.6923)
                ]
