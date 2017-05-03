{-# LANGUAGE DeriveGeneric #-}

module Spaceship where

import Reactive.Banana

import Geometry
import Reactive


makeSpaceship :: Event Vector -> Behavior Rectangle -> Tidings Rectangle
makeSpaceship v rectB =
  tidings (translateRectangle <$> rectB <@> v) rectB

spaceshipPoint :: Rectangle -> Vector
spaceshipPoint rect = vectorMidpoint v1 v2
  where
    v1 = rectangleA rect
    v2 = rectangleB rect
