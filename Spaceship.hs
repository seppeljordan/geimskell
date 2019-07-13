{-# LANGUAGE DeriveGeneric #-}

module Spaceship
  ( PlayerShip(..)
  , Health(..)
  , makeSpaceship
  , playerShipRectangle
  , translatePlayerShip
  , reducePlayerHealth
  , playerHealth
  , spaceshipPoint
  )
where

import Prelude hiding (Enum(..))
import Prelude.SafeEnum
import Reactive.Banana

import Geometry

data Health = HealthEmpty | HealthOne | HealthTwo | HealthThree
  deriving (Read,Show,Eq,Ord)

instance UpwardEnum Health where
  succ HealthEmpty = Just HealthOne
  succ HealthOne = Just HealthTwo
  succ HealthTwo = Just HealthThree
  succ HealthThree = Nothing
  HealthThree `succeeds` HealthThree = False
  HealthThree `succeeds` _ = True
  HealthTwo `succeeds` HealthThree = False
  HealthTwo `succeeds` HealthTwo = False
  HealthTwo `succeeds` _ = True
  HealthOne `succeeds` HealthEmpty = True
  HealthOne `succeeds` _ = False
  _ `succeeds` _ = False

instance DownwardEnum Health where
  pred HealthThree = Just HealthTwo
  pred HealthTwo = Just HealthOne
  pred HealthOne = Just HealthEmpty
  pred HealthEmpty = Nothing
  precedes x y = not (x == y) && not (succeeds x y)

data PlayerShip = PlayerShip
  { psArea :: Rectangle
  , psHealth :: Health
  }
  deriving (Show,Read,Eq)

reduceHealth :: Health -> Health
reduceHealth = maybe HealthEmpty id . pred

playerShipRectangle :: PlayerShip -> Rectangle
playerShipRectangle = psArea

playerHealth :: PlayerShip -> Health
playerHealth = psHealth

translatePlayerShip :: Vector -> PlayerShip -> PlayerShip
translatePlayerShip v ship@(PlayerShip { psArea = oldArea }) =
  ship { psArea = translateRectangle oldArea v}

makeSpaceship :: Event Vector
              -> Event (PlayerShip -> PlayerShip)
makeSpaceship v = translatePlayerShip <$> v

spaceshipPoint :: PlayerShip -> Vector
spaceshipPoint = rectangleMidpoint . psArea

reducePlayerHealth :: PlayerShip -> PlayerShip
reducePlayerHealth player@(PlayerShip { psHealth = health}) =
  player { psHealth = reduceHealth health }
