module Main where

--import Simplex

import Plotdata
import Datasaurus
import SimplicialComplex
import Numeric.LinearAlgebra as M
import Numeric.LinearAlgebra.Data as M

main :: IO ()
main = do
  let
    allShapes :: [SimplicialComplex]
    allShapes = [vLines4, sqr4, wedge4, gridShape, s1, hLines2, wedge, sqr, vLines2, xShape, hLines4]

    nFrames = 20
    nIters = 1500

    pcs0 = mkPointCloud' datasaurus vLines4 0
    pc0  = datasaurus
    pcl0  = datasaurus
    (pcl1,pc1)  = mkRAnimation' nFrames 1500 pcl0 pcs0


    pcs1 = mkPointCloud' pc1 sqr4 1
    (pcl2,pc2) = mkRAnimation' nFrames 2100 pcl1 pcs1
    --2:00

    pcs2 = mkPointCloud' pc2 wedge4 2
    (pcl3,pc3) = mkRAnimation' nFrames 1000 pcl2 pcs2
    --2:35

    pcs3 = mkPointCloud' pc3 gridShape 3
    (pcl4,pc4) = mkRAnimation' nFrames 1200 pcl3 pcs3
    --2:38

    pcs4 = mkPointCloud' pc4 s1 4
    (pcl5,pc5) = mkRAnimation' nFrames 1500 pcl4 pcs4
    --3:36 ;1750 4:30

    pcs5 = mkPointCloud' pc5 hLines2 5
    (pcl6,pc6) = mkRAnimation' nFrames 1500 pcl5 pcs5
    --3:20

    pcs6 = mkPointCloud' pc6 wedge 6
    (pcl7,pc7) = mkRAnimation' nFrames 1500 pcl6 pcs6
    --4:33

    pcs7 = mkPointCloud' pc7 sqr 7
    (pcl8,pc8) = mkRAnimation' nFrames 1500 pcl7 pcs7
    --4:38

    pcs8 = mkPointCloud' pc8 vLines2 8
    (pcl9,pc9) = mkRAnimation' nFrames 1000 pcl8 pcs8
    --13:43

    pcs9 = mkPointCloud' pc9 xShape 9
    (pcl10,pc10) = mkRAnimation' nFrames 1500 pcl9 pcs9

    pcs10 = mkPointCloud' pc10 hLines4 10
    (pcl11,pc11) = mkRAnimation' nFrames 1500 pcl10 pcs10

    addCol = rep (rows datasaurus) ([0..(20*11)] :: [R])
    pclf = pcl11 M.||| M.fromColumns [M.fromList addCol]

    outFile = "all" ++ ".csv"
  writeR outFile pclf
