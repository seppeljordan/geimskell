{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reactive
  ( Tidings
  , Game
  , Tick
  , Image
  , ResIndependentImage
  , Output(..)
  , EngineInputs (..)
  , GameNetwork (..)
  , RandomNetwork (..)
  , tidings
  , randomGenerator
  , screenWidth
  , screenHeight
  , unionsWith
  , buttonPressEvent
  , keyboardEvents
  , runNetworkWithOptions
  , facts
  , rumors
  , cooldownTimer
  , generateTicks
  , gameStage
  )
where

import Control.Concurrent.STM.TVar
import Data.Tuple
import Control.DeepSeq
import Control.Monad.Fix
import Control.Monad.STM
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State as State
import Data.Maybe
import Data.Text
import Foreign.C.Types
import GHC.Word
import Linear.V2 as L
import Reactive.Banana as RB
import Reactive.Banana.Frameworks as RB
import SDL hiding (clear, present)
import SDL.Compositor
import SDL.Compositor.ResIndependent
import System.Random as Rand

import Geimskell.Options
import Geimskell.Sound
import Stage
import TileSet

type Tick = ()

type ResIndependentImage = ResIndependent CompositingNode SDL.Texture
type Image = CompositingNode SDL.Texture

data Output =
  Output { outputImage :: Behavior Image
         , outputRenderTick :: RB.Event Tick
         , outputSounds :: RB.Event [SoundEffect]
         , outputRequestsQuit :: RB.Event ()
         }

instance Semigroup Output where
  a <> b = Output
    { outputImage = mappend <$> (outputImage a) <*> (outputImage b)
    , outputRenderTick =
        mappend (outputRenderTick a) (outputRenderTick b)
    , outputSounds =
        mappend (outputSounds a) (outputSounds b)
    , outputRequestsQuit =
        mappend (outputRequestsQuit a) (outputRequestsQuit b)
    }

instance Monoid Output where
  mempty = Output (pure mempty) never never never

data EngineInputs = EngineInputs { inputSdlEvents :: RB.Event EventPayload
                                 , inputTimeEvents :: RB.Event Word32
                                 , inputWindowSizeEvents :: RB.Event (SDL.V2 CInt)
                                 , inputStage :: Stage
                                 }

data Tidings a = Tidings { rumors :: RB.Event a
                         , facts :: Behavior a
                         }

instance Functor Tidings where
  fmap fun t = tidings (fmap fun $ rumors t) (fmap fun $ facts t)

instance Applicative Tidings where
  pure x = tidings never (pure x)
  (Tidings funE funB) <*> (Tidings xE xB) = tidings
    ( uncurry ($) <$> unionWith
      (\ (f,_) (_,x) -> (f,x))
      ( flip (,) <$> xB <@> funE )
      ( (,) <$> funB <@> xE )
    )
    ( funB <*> xB )

instance (Semigroup a) => Semigroup (Tidings a) where
  t1 <> t2 =
    (<>) <$> t1 <*> t2

instance Monoid a => Monoid (Tidings a) where
  mempty = tidings never (pure mempty)

cooldownTimer :: forall a.
                 RB.Behavior Bool
              -> RB.Behavior Word32 -- Timer duration in MicroSeconds
              -> RB.Event a
              -> Game (RB.Event a, RB.Behavior Word32)
cooldownTimer cooldownActiveB cooldownTimeB triggerE = do
  ticksInMillisecondsE <-
    filterApply (const <$> cooldownActiveB) <$>
    askTicksInMilliseconds
  let
    updateTimerState :: Word32 -> Word32 -> [Either Word32 a] -> State [a] Word32
    updateTimerState
      cooldownTime
      accuInMicroSeconds
      (Left tickInMilliseconds : xs)
      = updateTimerState
        cooldownTime
        (min (accuInMicroSeconds + tickInMilliseconds * 1000) cooldownTime)
        xs
    updateTimerState
      cooldownTime
      accuInMicroSeconds
      (Right triggerValue : xs)
      = if accuInMicroSeconds >= cooldownTime
        then modify (++ [triggerValue]) >> updateTimerState cooldownTime 0 xs
        else updateTimerState
             cooldownTime
             accuInMicroSeconds
             xs
    updateTimerState
      _
      accuInMicroSeconds
      ([])
      = return accuInMicroSeconds
    updateE :: RB.Event (Word32 -> (Maybe a, Word32))
    updateE =
      let
        triggerOrUpdateEvents :: RB.Event [Either Word32 a]
        triggerOrUpdateEvents = unionWith (++)
                                ((pure . Left) <$> ticksInMillisecondsE)
                                ((pure . Right) <$> triggerE)
      in ((\ cooldownTime events accu ->
            swap . fmap listToMaybe $
            runState (updateTimerState cooldownTime accu events) []
         ) <$> cooldownTimeB) <@> triggerOrUpdateEvents

  (maybeEventsE, timerB) <- mapAccum 0 updateE
  return (RB.filterJust maybeEventsE, timerB)

runNetworkWithOptions :: Text
                      -> Game Output
                      -> Options
                      -> IO ()
runNetworkWithOptions title action opts =
  serverBracket $ \ server -> runNetwork title action server gameRendererType
  where
    serverBracket = case optionsSoundEngine opts of
      OptionSoundEngineCsound -> withCsoundServer
      OptionSoundEngineNone -> withEmptySoundServer
    gameRendererType =
      case optionsRendererType opts of
        OptionRendererHardware -> AcceleratedRenderer
        OptionRendererSoftware -> SoftwareRenderer

-- | Take a game network and run it in a seprate window.
runNetwork :: Text -- ^ The title of the window to be created
           -> Game Output -- ^ the game network to run
           -> SoundServer
           -> RendererType
           -> IO ()
runNetwork title action server gameRendererType = do
  sdlHandler <- newAddHandler
  timeHandler <- newAddHandler
  windowSizeHandler <- newAddHandler
  window <- createWindow title defaultWindow
  renderer <-
    createRenderer window (-1)
    defaultRenderer { rendererType = gameRendererType }
  let
    resolution = V2 screenWidth screenHeight
  rendererLogicalSize renderer $= Just resolution
  requestsQuit <- newTVarIO False
  let
    exitOnFailure msg = error (msg ++ "\nExiting ...")
  inputStage <- fromMaybe (exitOnFailure "Failed to load Stage") <$> loadStage renderer
  putStrLn $ rnf inputStage `seq` "Stage Loaded"
  gen <- newStdGen
  let
    network = do
      inputSdlEvents <- fromAddHandler . fst $ sdlHandler
      inputTimeEvents <- fromAddHandler . fst $ timeHandler
      inputWindowSizeEvents <- fromAddHandler . fst $ windowSizeHandler
      output <- flip runReaderT
        (EngineInputs {..}) .
        runGameNetwork .
        flip evalStateT gen .
        runRandomNetwork $
        action
      let
        tickE = outputRenderTick output
        imageB = outputImage output
        soundEffectsE = outputSounds output
        requestsQuitE = outputRequestsQuit output
      RB.reactimate $ doRendering <$> (imageB <@ tickE)
      RB.reactimate $ mapM_ (playSoundEffect server) <$> soundEffectsE
      RB.reactimate $ (atomically $ writeTVar requestsQuit True) <$
        requestsQuitE
    doRendering image = do
      clear renderer
      runRenderer renderer (image :: Image)
      present renderer
    fireIfNotQuit (SDL.Event { eventPayload = ev })
      | ev == QuitEvent = return True
      | otherwise = (snd sdlHandler) ev >> return False
    eventLoop = do
      now <- ticks
      let go t0 = do
            sdlEvents <- pollEvents
            sdlQuit <- or <$> mapM fireIfNotQuit sdlEvents
            userQuit <- readTVarIO requestsQuit
            let
              abort = sdlQuit || userQuit
            t1 <- ticks
            (snd timeHandler) (t1 - t0)
            winSize <- SDL.get (windowSize window)
            (snd windowSizeHandler) winSize
            if abort then return () else go t1
      go now
  program <- compile network
  actuate program
  eventLoop
  destroyAssetCache (stageAssetCache inputStage)

screenWidth, screenHeight :: CInt
screenWidth = 1360
screenHeight = 768

sdlEventStream :: Game (RB.Event EventPayload)
sdlEventStream =
  lift
  (GameNetwork $ asks inputSdlEvents)

keyboardEvents :: Game (RB.Event KeyboardEventData)
keyboardEvents =
  filterJust . fmap filterKeyboardEvents <$> sdlEventStream
  where
    filterKeyboardEvents (KeyboardEvent ev) = Just ev
    filterKeyboardEvents _ = Nothing

buttonPressEvent :: Scancode -> KeyboardEventData -> Maybe Bool
buttonPressEvent
  desiredScancode
  (KeyboardEventData { keyboardEventRepeat = is_repeat
                     , keyboardEventKeysym = keysym
                     , keyboardEventKeyMotion = keymotion
                     })
  | (keysymScancode keysym) /= desiredScancode = Nothing
  | is_repeat = Nothing
  | otherwise = Just (keymotion == Pressed)

unionsWith :: (a -> a -> a) -> [RB.Event a] -> RB.Event a
unionsWith _ [] = never
unionsWith _ [x] = x
unionsWith fun (x:xs) = unionWith fun x (unionsWith fun xs)

askTicksInMilliseconds :: Game (RB.Event Word32)
askTicksInMilliseconds = lift . GameNetwork $ asks inputTimeEvents

-- | generateTicks takes as its argument the desired interval in micro
-- seconds.  60 FPS would be something like
--
-- > sixtyFPS <- generateTicks (pure 16666)
generateTicks :: (Integral i)
              => Behavior i
              -> Game (RB.Event i)
generateTicks deltaB = do
  ticksInMillisecondsE <- askTicksInMilliseconds
  lift . fmap (filterJust . fst) $
    mapAccum
    0
    ( (\ (delta,tx') accu' ->
         let
           tx = fromIntegral tx' * 1000
           accu = accu' + tx
         in
           if accu > delta
           then (Just delta, accu - delta)
           else (Nothing, accu)
      ) <$> ((,) <$> deltaB <@> ticksInMillisecondsE)
    )

gameStage :: Game Stage
gameStage = lift . GameNetwork $ asks inputStage

-- | @randomGenerator@ allows you to include "pseudo randomness" in
-- your game description.  We implement randomness by forking random
-- seeds from eachother and using these seeds as an accumulator when
-- generating random events.
randomGenerator :: RB.Event (StdGen -> (a,StdGen))
                -> Game (RB.Event a)
randomGenerator ev = do
  seed <- RandomNetwork State.get
  let
    (eventSeed, newSeed) = Rand.split seed
  RandomNetwork (put newSeed)
  fst <$> mapAccum eventSeed ev

newtype GameNetwork m a =
  GameNetwork { runGameNetwork :: ReaderT EngineInputs m a }
  deriving (Functor, Applicative, Monad, MonadFix)

instance MonadMoment m => MonadMoment (GameNetwork m) where
  liftMoment = GameNetwork . lift . liftMoment

instance MonadTrans GameNetwork where
  lift a = GameNetwork $ lift a

newtype RandomNetwork m a =
  RandomNetwork { runRandomNetwork :: StateT StdGen m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadTrans)

instance MonadMoment m => MonadMoment (RandomNetwork m) where
  liftMoment = RandomNetwork . lift . liftMoment

type Game = RandomNetwork (GameNetwork MomentIO)

tidings :: RB.Event a -> Behavior a -> Tidings a
tidings = Tidings

