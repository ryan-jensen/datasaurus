-- file: test/PointSpec.hs

module PointSpec where

import Test.Hspec
import Test.QuickCheck
import Point

------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "quadrance" $ do
    it "squared length of a vector/squared distance from origin to point" $ do
      quadrance p0 `shouldBe` 0
      quadrance p1 `shouldBe` 9
      quadrance p2 `shouldBe` 5
      quadrance p3 `shouldBe` 2
      quadrance p4 `shouldBe` 17
  describe "qd" $ do
    it "squared distance between two points" $ do
      qd p0 p1 `shouldBe` 9
      qd p1 p2 `shouldBe` 2
      qd p2 p1 `shouldBe` 2
      qd p3 p4 `shouldBe` 9
      qd p4 p4 `shouldBe` 0
  describe "norm" $ do
    it "length of a vector/distance from origin to point" $ do
      map norm [p0,p1,p2,p3,p4] `shouldBe` [0,3,sqrt 5, sqrt 2, sqrt 17]
  describe "distance" $ do
    it "distance between two vectors/points" $ do
      distance p0 p1 `shouldBe` 3
      distance p1 p2 `shouldBe` sqrt 2
      distance p4 p3 `shouldBe` 3

------------------------------------------------------------------------
prop_qdAssociative :: Point -> Point -> Bool
prop_qdAssociative x y = qd x y == qd y x

------------------------------------------------------------------------
p0,p1,p2,p3,p4 :: Point
p0 = [0,0]
p1 = [3,0]
p2 = [2,-1]
p3 = [1,1]
p4 = [4,1]

------------------------------------------------------------------------
