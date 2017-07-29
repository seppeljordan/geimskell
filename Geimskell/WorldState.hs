{-# LANGUAGE DeriveGeneric #-}

module Geimskell.WorldState where

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.List
import Data.Maybe
import GHC.Generics

import Camera
import Enemy
import Geometry
import Shoot
import Spaceship

data WorldState = WorldState { wsPlayer :: PlayerShip
                             , wsProjectiles :: [Projectile]
                             , wsEnemies :: [Enemy]
                             , wsCamera :: Camera
                             , wsInvincableRemainingMSeconds :: Int
                             }

data WorldUpdateEvent = EnemyDiedEvent Enemy
                      | ProjectileDestroyedEvent Projectile
  deriving Generic

type UpdateAction = StateT WorldState (Writer [WorldUpdateEvent])

modifyWorldState :: (WorldState -> WorldState) -> UpdateAction ()
modifyWorldState f = modify f

putWorldState ws = put ws

getWorldState = get

tellEvent :: WorldUpdateEvent -> UpdateAction ()
tellEvent e = lift . tell $ [e]

tellEvents = lift . tell

getProjectileDestroyedEvent :: WorldUpdateEvent -> Maybe Projectile
getProjectileDestroyedEvent (ProjectileDestroyedEvent p) = Just p
getProjectileDestroyedEvent _ = Nothing

isProjectileDestroyedEvent = isJust . getProjectileDestroyedEvent

outOfBounds :: Camera -> Rectangle -> Bool
outOfBounds camera rect =
      x <= (-5) || x > 5 || y <= (-5) || y > 5
      where
        x = pointX p - camPosition camera
        y = pointY p
        p = rectangleMidpoint rect

updateWorldState :: UpdateAction ()
                 -> WorldState
                 -> (WorldState, [WorldUpdateEvent])
updateWorldState action ws =
  runWriter (execStateT action ws)

updateTickW :: Int -> UpdateAction ()
updateTickW _ = return ()

updateProjectilesW :: [Projectile]
                   -> UpdateAction ()
updateProjectilesW updateProjectiles = do
  oldWS <- getWorldState
  let
    (destroyedProjectiles,newProjectiles) =
      partition
      (outOfBounds (wsCamera oldWS) . projectileRect)
      updateProjectiles
  tellEvents $ fmap ProjectileDestroyedEvent destroyedProjectiles
  putWorldState oldWS { wsProjectiles = newProjectiles }

updateEnemiesW :: ([Enemy] -> [Enemy])
               -> UpdateAction ()
updateEnemiesW updateEnemies = do
  oldWS <- getWorldState
  let
    (_, newEnemies) =
      partition
      (outOfBounds (wsCamera oldWS))
      (updateEnemies . wsEnemies $ oldWS)
  putWorldState oldWS { wsEnemies = newEnemies}


handleCollisionsW :: UpdateAction ()
handleCollisionsW = do
  ws <- getWorldState
  let
    enemies = wsEnemies ws
    projectiles = wsProjectiles ws
    (destroyedEnemies, newEnemies) =
      partition isProjectileCollision enemies
    (destroyedProjectiles, newProjectiles) =
      partition (isEnemyCollision . projectileRect) projectiles
    isEnemyCollision r =
      or . fmap (rectanglesOverlap r) $ enemies
    isProjectileCollision r =
      or . fmap (rectanglesOverlap r . projectileRect) $ projectiles
  tellEvents $ fmap EnemyDiedEvent destroyedEnemies
  tellEvents $ fmap ProjectileDestroyedEvent destroyedProjectiles
  putWorldState ws { wsProjectiles = newProjectiles
                   , wsEnemies = newEnemies
                   }

updatePlayerW :: (PlayerShip -> PlayerShip)
              -> UpdateAction ()
updatePlayerW f =
  modify $ \ ws -> ws { wsPlayer = f (wsPlayer ws) }

updateCameraW :: Camera
              -> UpdateAction ()
updateCameraW newCam =
  modify $ \ ws -> ws { wsCamera = newCam }
