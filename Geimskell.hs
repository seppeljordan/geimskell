{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Geimskell (main) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Tuple
import           GHC.Generics
import qualified Linear.V2 as L
import           Reactive.Banana as RB hiding ((<>))
import           SDL hiding (Rectangle, Vector, trace)
import           SDL.Compositor hiding
  (clear, present, createTexture, rendererRenderTarget)
import           SDL.Compositor.ResIndependent

import           Camera
import           Enemy
import           Geimskell.Options
import           Geimskell.Render
import           Geimskell.Sound
import           Geimskell.WorldState
import           Geometry
import           Reactive
import           Shoot
import           Spaceship

gameplay :: Behavior Bool -> RB.Event () -> Game Output
gameplay pauseB restartE = mdo
  keyboardE <- keyboardEvents
  stage <- gameStage
  let
    delta = 16666 :: Int
  ticks <- generateTicks (pure (delta :: Int))
  spawnTicks <- generateTicks (pure (1000 * 1000 :: Int))
  directionT <-
    controlls
    ScancodeUp ScancodeDown ScancodeLeft ScancodeRight
    =<< stepper (pure 0) (rumors directionT)
  let
    shootCooldown = pure $ 500 * 1000
    shootTriggerE = void $
      filterJust . fmap (buttonPressEvent ScancodeSpace) $
      keyboardE
  (shootE :: RB.Event (), _) <-
    cooldownTimer
    (not <$> gameStopB)
    shootCooldown
    (filterApply (const . not <$> gameStopB) shootTriggerE)
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
    explosionE = filterE (not . null . filter isEnemyDiedEvent) worldUpdateE
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
    projectilesB = wsProjectiles <$> worldStateB
    playerB = wsPlayer <$> worldStateB
    cameraB = wsCamera <$> worldStateB
    worldObjectImages = renderImage <$> (camPosition <$> cameraB) <*> worldStateB
    stageImage = renderImage <$>
                 (camPosition <$> cameraB) <*>
                 pure stage
    outputImage =
      mappend <$> stageImage <*> worldObjectImages
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

menu :: Game Output
menu = mdo
  keyboardE <- keyboardEvents
  let delta = 16000
  ticks <- generateTicks (pure (delta :: Int))
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
      ( translateR (L.V2 0.5 0.5) . flipC (L.V2 False True) <$> image
      )
      where
        image = startImage `combine` restartImage `combine` quitImage
        combine = liftA2 mappend
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
        (\ isPause menuImage gameImage ->
           if isPause then menuImage else gameImage ) <$>
        pauseB <*>
        oImage <*>
        outputImage gameOutput
      }
  return $ menuOutput

main :: IO ()
main = do
  getOptions >>= \case
    Left msg -> putStrLn msg
    Right opts ->
      withSdl $ runNetworkWithOptions "spaceshooter" menu opts

withSdl :: MonadIO m => m a -> m a
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

v2ToVector :: V2 Number -> Vector
v2ToVector (L.V2 x y) = makeVector x y

worldStateGameOver :: WorldState -> Bool
worldStateGameOver worldstate =
  (== HealthEmpty) . playerHealth . wsPlayer $ worldstate


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
