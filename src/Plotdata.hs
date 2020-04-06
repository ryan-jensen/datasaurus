{-# LANGUAGE OverloadedStrings #-}

module Plotdata where

import Data.Csv.HMatrix
import qualified Data.ByteString.Lazy as BS (writeFile)
import Datasaurus as D
import Point as P
import Simplex as S
import SimplicialComplex as SC
import Numeric.LinearAlgebra as M
import Numeric.LinearAlgebra.Data as M

mkRAnimation :: Int -> Int -> PointCloudState -> PointCloud
mkRAnimation nFrames skip pcs = concatpc M.||| M.fromColumns [M.fromList addCol]
  where
    initPC = pointCloud pcs
    pcl      = take nFrames $ every skip (iters pcs)
    concatpc = matConcat initPC pcl
    addCol   = rep (rows initPC) ([0.. (fromIntegral nFrames)] :: [R])

mkRAnimation' :: Int -> Int -> PointCloud -> PointCloudState -> (PointCloud, PointCloud)
mkRAnimation' nFrames skip initPC pcs = (concatpc, npc)
  --(concatpc M.||| M.fromColumns [M.fromList addCol], npc)
  where
    pcl      = take nFrames $ every skip (iters pcs)
    concatpc = matConcat initPC pcl
    npc      = head (reverse pcl)

rep = concatMap . replicate

every n xs = case drop (n-1) xs of
  (y:ys) -> y : every n ys
  [] -> []

writeR :: FilePath -> PointCloud -> IO ()
writeR fp pc = BS.writeFile fp (encodeMatrix pc)

matConcat :: PointCloud -> [PointCloud] -> PointCloud
matConcat x xs = foldl (M.===) x xs

