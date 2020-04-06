-- file: test/SimplexSpec.hs

module SimplexSpec where

import Test.Hspec
import Simplex
import Point
import Numeric.LinearAlgebra           hiding (fromList, fromLists, toList, toLists)
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Set              as Set

------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "valid" $ do
    it "decides if a simplex is valid" $ do
      valid s0 `shouldBe` True
      valid s1 `shouldBe` True
      valid s2 `shouldBe` True
      valid s3 `shouldBe` True
      valid s4 `shouldBe` True
      valid s5 `shouldBe` False

  describe "dim" $ do
    it "tells the dimension of a simplex" $ do
      dim empty `shouldBe` (-1)
      dim s0 `shouldBe` 0
      dim s1 `shouldBe` 1
      dim s2 `shouldBe` 2
      dim s3 `shouldBe` 2
      dim s4 `shouldBe` 2

  describe "qdToSimplex" $ do
    it "finds the quadrance from a point to a simplex" $ do
      qdToSimplex orig empty `shouldBe` (1/0)
      qdToSimplex orig s0 `shouldBe` 65
      qdToSimplex orig s1 `shouldBe` (49+16)
      qdToSimplex orig s2  `shouldSatisfy` (< 32.0000000001)
      qdToSimplex orig s2  `shouldSatisfy` (> 31.9999999999)
      qdToSimplex orig s3 `shouldBe` 10
      qdToSimplex orig s4 `shouldBe` 8

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
------------------------------------------------------------------------
