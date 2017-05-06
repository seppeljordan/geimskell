{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

import           Control.DeepSeq
import           Data.Monoid
import           GHC.Generics
import qualified Linear.V2 as L
import           Reactive.Banana as RB
import           SDL hiding (Rectangle, Vector)
import           SDL.Compositor hiding
  (clear, present, createTexture, rendererRenderTarget)
import           SDL.Compositor.ResIndependent
import           System.Random

import           Enemy
import           Geometry
import           Random
import           Reactive
import           Shoot
import           Spaceship
import           Sound

network = mdo
  keyboardE <- keyboardEvents
  let
    delta = 16666 :: Int
  ticks <- generateTicks (pure delta)
  spawnTicks <- generateTicks (pure (1000 * 1000 :: Int))
  directionT <-
    controlls
    ScancodeUp ScancodeDown ScancodeLeft ScancodeRight
    =<< stepper (pure 0) (rumors directionT)
  let
    shootCooldown = pure $ 100 * 1000
    shootTriggerE =
      filterJust . fmap (buttonPressEvent ScancodeSpace) $
      keyboardE
  shootTriggerB <- stepper False shootTriggerE
  shootE <-
    filterJust . fst <$> mapAccum 0
    ( (\ (cooldown, tryShoot) cooldownTimer' ->
          let
            cooldownTimer = min
                            (cooldownTimer' + delta) (cooldown + delta)
          in
            if tryShoot && cooldownTimer >= cooldown
            then (Just (), cooldownTimer - cooldown)
            else (Nothing, cooldownTimer)
      ) <$>
      ((,) <$> shootCooldown <*> shootTriggerB <@ ticks))
  enemiesT <- makeEnemies enemiesB spawnTicks
  let
    projectilesT =
      makeShoot
      (rectangleMidpoint <$> playerB)
      (pure (0.05,0.05))
      shootE
      projectilesB
      ticks
    playerT =
      makeSpaceship
      ( fmap
        (vectorScale 0.016 . v2ToVector)
        (facts directionT <@ ticks)
      )
      playerB
    outputRenderTick = () <$ ticks
    worldStateT =
      combineToWorldState <$>
      playerT <*>
      projectilesT <*>
      enemiesT
  worldStateB <-
    stepper
    ( WorldState { wsPlayer = initialSpaceship
                 , wsProjectiles = []
                 , wsEnemies = []
                 }
    ) $
    (rumors worldStateT)
  let
    enemiesB = wsEnemies <$> worldStateB
    projectilesB = wsProjectiles <$> worldStateB
    playerB = wsPlayer <$> worldStateB
  reactimate $ shootSound <$ shootE
  let
    outputImage = renderWorldState <$> worldStateB
  return $ Output {..}
  where
    initialSpaceship =
      makeRectangle
      (makeVector (-0.1) (-0.1))
      (makeVector 0.1 0.1)

red = rgba 255 0 0 255
blue = rgba 0 0 255 255
green = rgba 0 255 0 255

main = withSdl $ runNetwork "spaceshooter" network

withSdl action = do
  initializeAll
  result <- action
  quit
  return result

controlls :: (Num a) =>
             Scancode
          -> Scancode
          -> Scancode
          -> Scancode
          -> Behavior (V2 a)
          -> Game (Tidings (L.V2 a))
controlls
  scancodeUp
  scancodeDown
  scancodeLeft
  scancodeRight
  directionB =
  do
    keyboardE <- keyboardEvents
    let
      directionDefs = (,) <$>
        (Box4 scancodeUp scancodeDown scancodeLeft scancodeRight) <*>
        (Box4 (L.V2 0 1) (L.V2 0 (-1)) (L.V2 (-1) 0) (L.V2 1 0))
    directionBoxE <-
      accumE ((,) False <$> directionDefs ) $
      (\ keyboardEvent currentDirections ->
         fmap (\ (pressed, (sc, v)) ->
                case buttonPressEvent sc keyboardEvent of
                  Nothing -> (pressed, (sc, v))
                  Just newButtonState -> (newButtonState, (sc, v))
              )
         currentDirections
      ) <$> keyboardE
    let
      directionE =
        foldl
        (\ v (pressed, (_,d)) -> v + if pressed then d else pure 0)
        (pure 0) <$>
        directionBoxE
    return $ tidings directionE directionB

data Box4 a = Box4 a a a a
  deriving (Eq,Generic,Show,Read)

instance NFData a => NFData (Box4 a)

instance Functor Box4 where
  fmap f (Box4 a b c d) = Box4 (f a) (f b) (f c) (f d)

instance Applicative Box4 where
  pure x = Box4 x x x x
  (Box4 f1 f2 f3 f4) <*> (Box4 x1 x2 x3 x4) =
    Box4 (f1 x1) (f2 x2) (f3 x3) (f4 x4)

instance Foldable Box4 where
  foldl combine acc (Box4 a b c d) = foldl combine acc [a,b,c,d]
  foldr combine acc (Box4 a b c d) = foldr combine acc [a,b,c,d]

instance Traversable Box4 where
  traverse fun (Box4 a b c d) =
    Box4 <$> fun a <*> fun b <*> fun c <*> fun d

renderRectangle :: Color -> Rectangle -> Image
renderRectangle col rect =
  translateR offset (filledRectangleR (L.V2 width height) col)
  where
    width = abs $ (pointX . rectangleA $ rect) - (pointX . rectangleB $ rect)
    height = abs $ (pointY . rectangleA $ rect) - (pointY . rectangleB $ rect)
    offset =
      L.V2
      (avg (pointX . rectangleA $ rect) (pointX . rectangleB $ rect))
      (avg (pointY . rectangleA $ rect) (pointY . rectangleB $ rect))
    avg a b = (a+b)/2

translateRVector :: Vector -> (Image -> Image)
translateRVector = translateR . vectorToV2

vectorToV2 v = L.V2 x y
  where
    x = pointX v
    y = pointY v
v2ToVector (L.V2 x y) = makeVector x y

handleCollisions projectiles enemies =
  (newProjectiles, newEnemies)
  where
    newProjectiles = filter (not . isEnemyCollision . projectileRect)
                     projectiles
    newEnemies = filter (not . isProjectileCollision) enemies
    isEnemyCollision r =
      or . map (rectanglesOverlap r) $ enemies
    isProjectileCollision r =
      or . map (rectanglesOverlap r . projectileRect) $ projectiles

deleteProjectile p =
  let v = projectilePosition p
      x = pointX v
      y = pointY v
  in x >= (-5) && x <= 5 && y >= (-5) && y <= 5

type Player = Rectangle

data WorldState = WorldState { wsPlayer :: Player
                             , wsProjectiles :: [Projectile]
                             , wsEnemies :: [Enemy]
                             }

combineToWorldState :: Player
                    -> [Projectile]
                    -> [Enemy]
                    -> WorldState
combineToWorldState player projectiles enemies =
  WorldState
  { wsPlayer = player
  , wsProjectiles = newProjectiles
  , wsEnemies = newEnemies
  }
  where
    (newProjectiles, newEnemies) =
      handleCollisions projectiles enemies

renderWorldState (WorldState { wsPlayer = player
                             , wsEnemies = enemies
                             , wsProjectiles = projectiles }) =
  outputImage
  where
    spaceshipGraphics = renderRectangle blue player
    projectilesImage =
      mconcat . map (renderRectangle red . projectileRect) $
      projectiles
    enemiesImage =
      mconcat . map (renderRectangle green) $
      enemies
    background = mempty
    outputImage =
      translateR (L.V2 0.5 0.5) .
      flipC (L.V2 False True) $
      ( background <>
        enemiesImage <>
        projectilesImage <>
        spaceshipGraphics
      )
