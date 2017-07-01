{-# LANGUAGE DeriveGeneric #-}

module Spaceship where

import Reactive.Banana

import Geometry
import Reactive

data Health = HealthEmpty | HealthOne | HealthTwo | HealthThree
  deriving (Read,Show,Eq,Ord,Enum)

data PlayerShip = PlayerShip
  { psArea :: Rectangle
  , psHealth :: Health
  }
  deriving (Show,Read,Eq)

playerShipRectangle = psArea

translatePlayerShip :: Vector -> PlayerShip -> PlayerShip
translatePlayerShip v ship@(PlayerShip { psArea = oldArea }) =
  ship { psArea = translateRectangle oldArea v}

makeSpaceship :: Event Vector -> Behavior PlayerShip -> Tidings PlayerShip
makeSpaceship v shipB =
  tidings (flip translatePlayerShip <$> shipB <@> v) shipB

spaceshipPoint :: PlayerShip -> Vector
spaceshipPoint = rectangleMidpoint . psArea
