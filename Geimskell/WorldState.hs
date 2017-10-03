{-# LANGUAGE DeriveGeneric #-}

module Geimskell.WorldState
  ( -- * WorldState
    WorldState
  , mkWorldState
  , wsPlayer
  , wsProjectiles
  , wsEnemies
  , wsCamera
  -- * Updating WorldState
  , UpdateAction
  , updateWorldState
  , updatePlayerW
  , updateEnemiesW
  , updateProjectilesW
  , updateTickW
  , updateCameraW
  , putWorldState
  -- * Notifications to the engine
  , WorldUpdateEvent
  )

where

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
                             , wsInvincibleRemainingSeconds :: Int
                             }

mkWorldState :: PlayerShip -> [Projectile] -> [Enemy] -> Camera -> WorldState
mkWorldState player projectiles enemies camera =
  WorldState { wsPlayer = player
             , wsProjectiles = projectiles
             , wsEnemies = enemies
             , wsCamera = camera
             , wsInvincibleRemainingSeconds = 0
             }

data WorldUpdateEvent = EnemyDiedEvent Enemy
                      | ProjectileDestroyedEvent Projectile
  deriving Generic

newtype UpdateAction a =
  UpdateAction
  {fromUpdateAction
   :: StateT WorldState (Writer [WorldUpdateEvent]) a}

instance Functor UpdateAction where
  fmap f (UpdateAction a) = UpdateAction $ f <$> a

instance Applicative UpdateAction where
  pure v = UpdateAction $ pure v
  (<*>) (UpdateAction f) (UpdateAction x) = UpdateAction $ f <*> x

instance Monad UpdateAction where
  return = pure
  (>>=) action generator = UpdateAction $ fromUpdateAction action >>= fromUpdateAction . generator

modifyWorldState :: (WorldState -> WorldState) -> UpdateAction ()
modifyWorldState f = UpdateAction $ modify f

putWorldState ws = UpdateAction $ put ws

getWorldState = UpdateAction $ get

tellEvent :: WorldUpdateEvent -> UpdateAction ()
tellEvent e = UpdateAction . lift . tell $ [e]

tellEvents = UpdateAction . lift . tell

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
updateWorldState (UpdateAction action) ws =
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
  handleEnemyProjectileCollisions

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
  handleEnemyProjectileCollisions
  handlePlayerEnemyCollisions


handleEnemyProjectileCollisions :: UpdateAction ()
handleEnemyProjectileCollisions = do
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

handlePlayerEnemyCollisions :: UpdateAction ()
handlePlayerEnemyCollisions = do
  ws <- getWorldState
  let
    player = wsPlayer ws
    (hitEnemies, nonHitEnemies) =
      partition
      (rectanglesOverlap $ playerShipRectangle player)
      (wsEnemies ws)
    newPlayer = if null hitEnemies
                then player
                else reducePlayerHealth player
  tellEvents $ fmap EnemyDiedEvent hitEnemies
  putWorldState ws
    { wsPlayer = newPlayer
    , wsEnemies = nonHitEnemies }

updatePlayerW :: Number
              -> (PlayerShip -> PlayerShip)
              -> UpdateAction ()
updatePlayerW screenWidth f = do
  modifyWorldState $ \ ws -> ws { wsPlayer = f (wsPlayer ws) }
  handlePlayerBorders screenWidth
  handlePlayerEnemyCollisions

handlePlayerBorders screenWidth = do
  ws <- getWorldState
  let
    oldPlayer = wsPlayer ws
    camera = camPosition $ wsCamera ws
    playerRect = psArea oldPlayer
    correctionX =
      max 0 ((camera - screenWidth/2) - pointX (rectangleA playerRect)) +
      min 0 ((camera + screenWidth/2) - pointX (rectangleB playerRect))
    correctionY =
      max 0 ((-0.5) - pointY (rectangleA playerRect)) +
      min 0 (0.5 - pointY (rectangleB playerRect))
    newPlayer = translatePlayerShip
                (makeVector correctionX correctionY)
                oldPlayer
  putWorldState ws { wsPlayer = newPlayer }

updateCameraW :: Camera
              -> UpdateAction ()
updateCameraW newCam =
  modifyWorldState $ \ ws -> ws { wsCamera = newCam }
