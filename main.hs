{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

import           Control.DeepSeq
import           Control.Monad
import           Data.Array
import           Data.Monoid
import           GHC.Generics
import qualified Linear.V2 as L
import           Reactive.Banana as RB
import           SDL hiding (Rectangle, Vector, trace)
import           SDL.Compositor hiding
  (clear, present, createTexture, rendererRenderTarget)
import           SDL.Compositor.ResIndependent
import           System.Random

import           Camera
import           Enemy
import           Geometry
import           Random
import           Reactive
import           Shoot
import           Sound
import           Spaceship
import           Stage

gameplay pauseB = mdo
  keyboardE <- keyboardEvents
  stage <- gameStage
  let
    delta = 16666 :: Int
  ticks <- generateTicks (pure delta)
  spawnTicks <- generateTicks (pure (1000 * 1000 :: Int))
  directionT <-
    controlls
    ScancodeUp ScancodeDown ScancodeLeft ScancodeRight
    =<< stepper (pure 0) (rumors directionT)
  let
    shootCooldown = pure $ 500 * 1000
    shootTriggerE =
      filterJust . fmap (buttonPressEvent ScancodeSpace) $
      keyboardE
  shootTriggerB <- stepper False shootTriggerE
  shootE <-
    whenE (not <$> pauseB) . filterJust . fst <$> mapAccum 0
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
  enemiesT <- makeEnemies (camPosition <$> cameraB) enemiesB spawnTicks ticks
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
      ( apply
        ((\ speed d -> vectorAdd d (makeVector speed 0)) <$>
         (camSpeed <$> cameraB)
        ) .
        fmap (vectorScale 0.016 . v2ToVector) $
        (facts directionT <@ ticks)
      )
      playerB
    cameraT = makeCamera ticks cameraB
    outputRenderTick = () <$ ticks
    worldStateT =
      combineToWorldState <$>
      cameraT <*>
      playerT <*>
      projectilesT <*>
      enemiesT
    explosionE =
      fmap (const ()) .
      filterE (>0) .
      fmap snd .
      rumors $
      worldStateT
  worldStateB <-
    stepper
    initialWorldState $
    (whenE (not <$> pauseB) $ fst <$> rumors worldStateT)
  let
    enemiesB = wsEnemies <$> worldStateB
    projectilesB = wsProjectiles <$> worldStateB
    playerB = wsPlayer <$> worldStateB
    cameraB = wsCamera <$> worldStateB
    worldObjectImages = renderWorldState <$> (camPosition <$> cameraB) <*> worldStateB
    stageImage = renderStage <$>
                 (camPosition <$> cameraB) <*>
                 pure stage
    outputImage =
      stageImage <>
      (fromRelativeCompositor (L.V2 768 600) <$> worldObjectImages)
    outputSounds = unionWith (++)
      ([SoundShoot] <$ shootE)
      ([SoundExplosion] <$ explosionE)
    outputRequestsQuit = never
  return $ Output {..}
  where
    initialWorldState =
      WorldState { wsPlayer = initialSpaceship
                 , wsProjectiles = []
                 , wsEnemies = []
                 , wsCamera = Camera { camSpeed = 0.001
                                     , camPosition = 0
                                     }
                 }
    initialSpaceship =
      makeRectangle
      (makeVector (-0.1) (-0.1))
      (makeVector 0.1 0.1)

data Menu = MenuStart | MenuQuit
  deriving Eq

menu = mdo
  keyboardE <- keyboardEvents
  let delta = 16000
  ticks <- generateTicks (pure delta)
  let
    arrowUpE =
      filterE id . filterJust $
      buttonPressEvent ScancodeUp <$> keyboardE
    arrowDownE =
      filterE id . filterJust $
      buttonPressEvent ScancodeDown <$> keyboardE
    enterE = filterE id . filterJust $
      buttonPressEvent ScancodeReturn <$> keyboardE
    escapeE = void . filterE id . filterJust $
      buttonPressEvent ScancodeEscape <$> keyboardE
  menuSelection <- stepper MenuStart . whenE pauseB $
    unionWith const
    (MenuStart <$ arrowUpE)
    (MenuQuit <$ arrowDownE)
  let
    startImage = renderRectangle <$>
      ((\m -> case m of
           MenuStart -> red
           MenuQuit -> blue
       ) <$> menuSelection) <*>
      pure (makeRectangle
            (makeVector (-0.4) 0.1)
            (makeVector (0.4) 0.4))
    quitImage = renderRectangle <$>
      ((\m -> case m of
           MenuStart -> blue
           MenuQuit -> red
       ) <$> menuSelection) <*>
      pure ( makeRectangle
             (makeVector (-0.4) (-0.4))
             (makeVector (0.4) (-0.1)))
    oImage = fromRelativeCompositor (L.V2 768 600) <$>
      ( translateR (L.V2 0.5 0.5) <$> startImage <> quitImage )
    oSounds = never
    oRenderTick = ticks
    exitButtonE =
      void . whenE pauseB . filterE (== MenuQuit) $
      menuSelection <@ enterE
    startButtonE =
      void . whenE pauseB . filterE (== MenuStart) $
      menuSelection <@ enterE
    oRequestsQuit = exitButtonE
  pauseB <- accumB True $ not <$ unionsWith const [escapeE,startButtonE]
  gameOutput <- gameplay pauseB
  let
    menuOutput = Output
      { outputSounds = oSounds <> outputSounds gameOutput
      , outputRenderTick = oRenderTick
      , outputRequestsQuit = oRequestsQuit
      , outputImage =
        (\ pause menu game ->
           if pause then menu else game ) <$>
        pauseB <*>
        oImage <*>
        outputImage gameOutput
      }
  return $ menuOutput

red = rgba 255 0 0 255
blue = rgba 0 0 255 255
green = rgba 0 255 0 255

main = withSdl $ runNetwork "spaceshooter" menu

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

renderRectangle :: Color -> Rectangle -> ResIndependentImage
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

translateRVector :: Vector
                 -> (ResIndependentImage -> ResIndependentImage)
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

type Player = Rectangle

data WorldState = WorldState { wsPlayer :: Player
                             , wsProjectiles :: [Projectile]
                             , wsEnemies :: [Enemy]
                             , wsCamera :: Camera
                             }

combineToWorldState camera player projectiles enemies =
  ( WorldState
    { wsPlayer = player
    , wsProjectiles =
      filter (not . outOfBounds . projectileRect ) newProjectiles
    , wsEnemies =
      filter (not . outOfBounds) newEnemies
    , wsCamera = camera
    }
  , length enemies - length newEnemies
  )
  where
    (newProjectiles, newEnemies) =
      handleCollisions projectiles enemies
    outOfBounds rect =
      x <= (-5) || x > 5 || y <= (-5) || y > 5
      where
        x = pointX p - camPosition camera
        y = pointY p
        p = rectangleMidpoint rect

renderWorldState xPosition
                 (WorldState { wsPlayer = player
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
    outputImage =
      translateR (L.V2 (-xPosition) 0) .
      translateR (L.V2 0.5 0.5) .
      flipC (L.V2 False True) $
      ( enemiesImage <>
        projectilesImage <>
        spaceshipGraphics
      )

renderStage :: Number -> Stage -> Image
renderStage camPosition stage =
  mconcat . fmap makeImage . filter onScreen . assocs $ (stageData stage)
  where
    camPositionAbsolute = round $ camPosition * 600
    tileWidth = 32 :: Int
    tileHeight = 32 :: Int
    relativeWidth = 1/fromIntegral tileWidth :: Number
    makeImage (_,Nothing) = mempty
    makeImage ((x,y), Just tex) =
      ( translateA
        ( L.V2
          (x * tileWidth - camPositionAbsolute)
          (y * tileHeight)
        )
      ) $
      sizedA (L.V2 tileWidth tileHeight) tex
    onScreen ((gridXPos,_),_) =
      -- gridXPos is just the position in the tilegrid and has nothing
      -- to do with the actual position on the screen
      x * relativeWidth > camPosition - 2 &&
      x * relativeWidth < camPosition + 2
      where
        x = fromIntegral gridXPos * relativeWidth
