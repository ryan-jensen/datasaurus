{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module SimplicialComplex
  ( SimplicialComplex
  , mkSimplicialComplex
  , qdToSC
  , scFromLists
  , mySC
  , unSimplicialComplex
  ) where

------------------------------------------------------------------------
import Numeric.LinearAlgebra           hiding (fromList, fromLists)
import qualified Numeric.LinearAlgebra as LA
import Foreign.Storable
import Point
import Simplex as S

------------------------------------------------------------------------
-- Store the maximal simplicies
data SimplicialComplex = SC Int [Simplex] deriving (Show, Read, Eq)

------------------------------------------------------------------------
mkSimplicialComplex = SC

validSC (SC (-1) []) = True
validSC (SC n ss) = all valid ss
-- TODO: Check that intersections work, will need intersection of
--       simplicies for this

qdToSC :: Point -> SimplicialComplex -> R
qdToSC p (SC _ ss) = minimum $ (map (qdToSimplex p)) ss

scFromLists' :: Int -> [[Point]] -> SimplicialComplex
scFromLists' n pts = SC n  (fmap S.fromList pts)

scFromLists :: [[Point]] -> SimplicialComplex
scFromLists pts = scFromLists' n pts
  where n = (length . head) pts - 1

unSimplicialComplex :: SimplicialComplex -> (Int, [Simplex])
unSimplicialComplex (SC dim ss) = (dim, ss)

------------------------------------------------------------------------
p0 :: Point
p0 = [0,0]

p1 :: Point
p1 = [10,10]

p2 :: Point
p2 = [10,1]

s0 :: Simplex
s0 = simplexFromList [[0,0],[1,1]]

s1 :: Simplex
s1 = fromList [[1,1],[2,1],[1.5,2]]

mySC = SC 2 [s0,s1]

------------------------------------------------------------------------
