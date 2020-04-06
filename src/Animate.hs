{-# LANGUAGE OverloadedStrings #-}

module Animate where

import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD.Text as D
import qualified Reanimate.Diagrams as A
import qualified Reanimate as A
import qualified Reanimate.Render as A

import Data.Csv.HMatrix
import qualified Data.ByteString.Lazy as BS (writeFile)
import Datasaurus as D
import Point as P
import Simplex as S
import SimplicialComplex as SC
import Numeric.LinearAlgebra as M
import Numeric.LinearAlgebra.Data as M

mkRAnimation :: Int -> Int -> PointCloudState -> PointCloud
mkRAnimation nFrames skip pcs =  concatpc M.||| M.fromColumns [M.fromList addCol]
  where
    initPC = pointCloud pcs
    pcl      = take nFrames $ every skip (iters pcs)
    concatpc = matConcat initPC pcl
    addCol   = rep (rows initPC) ([0.. (fromIntegral nFrames)] :: [R])

rep = concatMap . replicate

every n xs = case drop (n-1) xs of
  (y:ys) -> y : every n ys
  [] -> []

writeR :: FilePath -> PointCloud -> IO ()
writeR fp pc = BS.writeFile fp (encodeMatrix pc)

matConcat :: PointCloud -> [PointCloud] -> PointCloud
matConcat x xs = foldl (M.===) x xs

main :: IO ()
main = A.render drawCirc "test.mp4" A.RenderMp4 800 800 24

drawProgress' :: A.Animation
drawProgress' = A.mkAnimation 2 $ \t ->
  A.mkGroup
  [ A.mkLine (-A.screenWidth/2*widthP,0)
           (A.screenWidth/2*widthP,0)
  , A.translate (-A.screenWidth/2*widthP + A.screenWidth*widthP*t) 0 $
    A.withFillOpacity 1 $ A.mkCircle 0.5 ]
  where
    widthP = 0.8

drawProgress = docEnv drawProgress'

docEnv :: A.Animation -> A.Animation
docEnv = A.mapA $ \svg -> A.mkGroup
  [ A.mkBackground "white"
  , A.withFillOpacity 0 $
    A.withStrokeWidth 0.1 $
    A.withStrokeColor "black" (A.mkGroup [svg]) ]

drawCirc = A.mkAnimation 2 $ \t ->
  A.mkGroup
  [ A.mkBackground "white"
  , A.scale (2/50) $ A.center $ A.withStrokeColor "black" $
    A.renderDiagram $ D.withEnvelope (D.rect 320 180 :: A.SvgDiagram) $
    D.scale 50 $
    D.lc D.black $
    myCirc t ]

myCirc :: Double -> A.SvgDiagram
myCirc n = D.hsep  1 [ D.circle m
                     , D.circle 0.1
                     , D.circle 1
                     , D.square m
                     , D.triangle m
                     ]
--                     , D.text (show n) D.<> D.square 1 D.# D.fc D.white]
  D.#D.fc D.black
  where m = n+0.1
