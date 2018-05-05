{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Geimskell where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Trans.Writer
import           Data.Array
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import           Data.Tuple
import           GHC.Generics
import qualified Linear.V2 as L
import           Reactive.Banana as RB hiding ((<>))
import           SDL hiding (Rectangle, Vector, trace)
import           SDL.Compositor hiding
  (clear, present, createTexture, rendererRenderTarget)
import           SDL.Compositor.ResIndependent
import           System.Random

import           Camera
import           Enemy
import           Geimskell.Options
import           Geimskell.WorldState
import           Geometry
import           Random
import           Reactive
import           Shoot
import           Sound
import           Spaceship
import           Stage
import           TileSet

gameplay pauseB restartE = mdo
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
  transformEnemiesE <- makeEnemies (camPosition <$> cameraB) (void spawnTicks) ticks
  let
    projectilesT =
      makeShoot
      (spaceshipPoint <$> playerB)
      (pure (0.05,0.05))
      shootE
      projectilesB
      (void ticks)
    playerTransformationsE =
      makeSpaceship
      ( apply
        ((\ speed d -> vectorAdd d (makeVector speed 0)) <$>
         (camSpeed <$> cameraB)
        ) .
        fmap (vectorScale 0.016 . v2ToVector) $
        (facts directionT <@ ticks)
      )
    cameraT = makeCamera (void ticks) cameraB
    outputRenderTick = void ticks
    explosionE = filterE (not . null) worldUpdateE
  (worldUpdateE, worldStateB) <- combineWorldState
    initialWorldState
    gameStopB
    ticks
    (rumors cameraT)
    playerTransformationsE
    transformEnemiesE
    (rumors projectilesT)
    restartE
  let
    gameStopB =
      (||) <$>
      pauseB <*>
      gameOverB
    gameOverB = worldStateGameOver <$> worldStateB
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
      (fromRelativeCompositor (fromIntegral <$> L.V2 screenWidth screenHeight) <$> worldObjectImages)
    outputSounds = unionWith (++)
      ([SoundShoot] <$ shootE)
      ([SoundExplosion] <$ explosionE)
    outputRequestsQuit = never
  return $ Output {..}
  where
    initialWorldState =
      mkWorldState
      initialSpaceship
      []
      []
      Camera { camSpeed = 0.001
             , camPosition = 0
             }
    initialSpaceship =
      PlayerShip { psArea = makeRectangle
                   (makeVector (-0.1) (-0.1))
                   (makeVector 0.1 0.1)
                 , psHealth = HealthThree
                 }

data Menu = MenuStart | MenuRestart | MenuQuit
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
  menuSelection <- accumB MenuStart . whenE pauseB $
    let
      selectionUp MenuStart = MenuStart
      selectionUp MenuRestart = MenuStart
      selectionUp MenuQuit = MenuRestart
      selectionDown MenuStart = MenuRestart
      selectionDown MenuRestart = MenuQuit
      selectionDown MenuQuit = MenuQuit
    in
      unionWith (.)
      (selectionUp <$ arrowUpE)
      (selectionDown <$ arrowDownE)
  let
    startImage = renderRectangle <$>
      ((\case
           MenuStart -> red
           _ -> blue
       ) <$> menuSelection) <*>
      pure (makeRectangle
            (makeVector (-0.4) 0.15)
            (makeVector (0.4) 0.4))
    restartImage = renderRectangle <$>
      ((\case
           MenuRestart -> red
           _ -> blue
       ) <$> menuSelection) <*>
      pure (makeRectangle
            (makeVector (-0.4) 0.1)
            (makeVector (0.4) (-0.1)))
    quitImage = renderRectangle <$>
      ((\case
           MenuQuit -> red
           _ -> blue
       ) <$> menuSelection) <*>
      pure ( makeRectangle
             (makeVector (-0.4) (-0.4))
             (makeVector (0.4) (-0.15)))
    oImage = fromRelativeCompositor (fromIntegral <$> L.V2 screenWidth screenHeight) <$>
      ( translateR (L.V2 0.5 0.5) . flipC (L.V2 False True) <$>
        startImage <> restartImage <> quitImage
      )
    oSounds = never
    oRenderTick = void ticks
    exitButtonE =
      void . whenE pauseB . filterE (== MenuQuit) $
      menuSelection <@ enterE
    startButtonE =
      void . whenE pauseB . filterE (== MenuStart) $
      menuSelection <@ enterE
    restartSelectionE =
      void . whenE pauseB . filterE (== MenuRestart) $
      menuSelection <@ enterE
    oRequestsQuit = exitButtonE
  pauseB <- accumB True $ not <$ unionsWith const [escapeE,startButtonE,restartSelectionE]
  gameOutput <- gameplay pauseB restartSelectionE
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

main = do
  getOptions >>= \case
    Left msg -> putStrLn msg
    Right opts ->
      withSdl $ runNetworkWithOptions "spaceshooter" menu opts

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

renderWorldState xPosition worldstate =
  outputImage
  where
    player = wsPlayer worldstate
    projectiles = wsProjectiles worldstate
    enemies = wsEnemies worldstate
    spaceshipGraphics = renderRectangle blue $ playerShipRectangle player
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

worldStateGameOver worldstate =
  (== HealthEmpty) . playerHealth . wsPlayer $ worldstate

renderStage :: Number -> Stage -> Image
renderStage camPosition stage =
  mconcat . fmap makeImage . filter onScreen . assocs $ (stageData stage)
  where
    camPositionAbsolute = round $ camPosition * 600
    tileWidth = 32 :: Int
    tileHeight = 32 :: Int
    relativeWidth = 1/fromIntegral tileWidth :: Number
    makeImage (_,Nothing) = mempty
    makeImage ((x,y), Just tile) =
      ( translateA
        ( L.V2
          (x * tileWidth - camPositionAbsolute)
          (y * tileHeight + tileHeight `div` 2)
        )
      ) $
      sizedA (L.V2 tileWidth tileHeight) (tileTexture tile)
    onScreen ((gridXPos,_),_) =
      -- gridXPos is just the position in the tilegrid and has nothing
      -- to do with the actual position on the screen
      x * relativeWidth > camPosition - 2 &&
      x * relativeWidth < camPosition + 2
      where
        x = fromIntegral gridXPos

combineWorldState :: WorldState
                  -> Behavior Bool
                  -> RB.Event Int
                  -> RB.Event Camera
                  -> RB.Event (PlayerShip -> PlayerShip)
                  -> RB.Event ([Enemy] -> [Enemy])
                  -> RB.Event [Projectile]
                  -> RB.Event ()
                  -> Game ( RB.Event [WorldUpdateEvent]
                          , Behavior WorldState)
combineWorldState initialWorldState isPauseB tickE camE playerE enemiesE
  projectilesE resetE =
  mapAccum initialWorldState $
  (\action -> swap . updateWorldState action) <$> updatesE
  where
    updatesE :: RB.Event (UpdateAction ())
    updatesE = unionsWith (>>)
      [ whenE (not <$> isPauseB) gameplayUpdates
      , extraEvents
      ]
    extraEvents = putWorldState initialWorldState <$ resetE
    gameplayUpdates = unionsWith (>>)
      [ updateTickW <$> tickE
      , updateProjectilesW <$> projectilesE
      , updateEnemiesW <$> enemiesE
      , updatePlayerW
        (fromIntegral screenWidth / fromIntegral screenHeight) <$>
        playerE
      , updateCameraW <$> camE
      ]
