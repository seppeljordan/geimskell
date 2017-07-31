{-# LANGUAGE DeriveGeneric #-}

module Geometry
  ( Vector, makeVector, Rectangle, makeRectangle, Number
  , rectangleA, rectangleB, vectorMidpoint, translateRectangle
  , rectangleMidpoint, pointX, pointY, vectorScale, rectanglesOverlap
  , vectorAdd
  )

where

import GHC.Generics

type Number = Float

data Vector = Vector { pointX :: Number
                     , pointY :: Number
                     }
            deriving (Show,Read,Eq,Generic)

instance Ord Vector where
  compare v1 v2 = case compare (pointX v1) (pointY v2) of
    EQ -> compare (pointY v1) (pointY v2)
    v -> v

makeVector = Vector

vectorMidpoint :: Vector -> Vector -> Vector
vectorMidpoint (Vector {pointX = v1x, pointY = v1y })
               (Vector {pointX = v2x, pointY = v2y }) =
  makeVector
  ((v1x + v2x) / 2)
  ((v1y + v2y) / 2)

vectorAdd :: Vector -> Vector -> Vector
vectorAdd (Vector v1 v2) (Vector w1 w2) =
  Vector (v1 + w1) (v2 + w2)

vectorScale :: Number -> Vector -> Vector
vectorScale r (Vector x y) = Vector (r*x) (r*y)

data Rectangle = Rectangle { rectangleA :: Vector
                           , rectangleB :: Vector
                           }
                 deriving (Show,Read,Eq)

instance Ord Rectangle where
  compare r1 r2 =
    case compare (rectangleA r1) (rectangleA r2) of
      EQ -> compare (rectangleB r1) (rectangleB r2)
      v -> v

makeRectangle (Vector ax ay) (Vector bx by) =
  Rectangle
  (Vector (min ax bx) (min ay by))
  (Vector (max ax bx) (max ay by))

translateRectangle :: Rectangle -> Vector -> Rectangle
translateRectangle (Rectangle a b) v =
  Rectangle (a `vectorAdd` v) (b `vectorAdd` v)

rectangleMidpoint (Rectangle v w) = vectorMidpoint v w

rectanglesOverlap
  (Rectangle (Vector vx vy) (Vector wx wy))
  (Rectangle (Vector ax ay) (Vector bx by)) =
  xcollision && ycollision
  where
    xcollision = wx >= ax && bx >= vx
    ycollision = wy >= ay && by >= vy

rectangleHeight :: Rectangle -> Number
rectangleHeight r =
  abs $ pointY q - pointY p
  where
    p = rectangleA r
    q = rectangleB r
