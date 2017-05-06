{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reactive where

import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State as State
import Data.Text
import GHC.Word
import Linear.V2 as L
import Reactive.Banana as RB
import Reactive.Banana.Frameworks as RB
import SDL hiding (clear, present)
import SDL.Compositor
import SDL.Compositor.ResIndependent
import System.Random as Rand

import Sound

type Tick = ()

type Image = ResIndependent CompositingNode SDL.Texture

data Output =
  Output { outputImage :: Behavior Image
         , outputRenderTick :: RB.Event Tick
         , outputSounds :: RB.Event [SoundEffect]
         }

data EngineInputs = EngineInputs { inputSdlEvents :: RB.Event EventPayload
                                 , inputTimeEvents :: RB.Event Word32
                                 }

-- | Take a game network and run it in a seprate window.
runNetwork :: Text -- ^ The title of the window to be created
           -> Game Output -- ^ the game network to run
           -> IO ()
runNetwork title action = withSoundServer $ \ server -> do
  sdlHandler <- newAddHandler
  timeHandler <- newAddHandler
  window <- createWindow title defaultWindow
  renderer <-
    createRenderer window (-1) defaultRenderer { rendererType = AcceleratedVSyncRenderer }
  let
    resolution = V2 800 600
    resolutionLinear = L.V2 800 600 :: L.V2 Int
  rendererLogicalSize renderer $= Just resolution
  gen <- newStdGen
  let
    network = do
      sdlEvents <- fromAddHandler . fst $ sdlHandler
      timeEvent <- fromAddHandler . fst $ timeHandler
      output <- flip runReaderT
        (EngineInputs { inputSdlEvents = sdlEvents
                      , inputTimeEvents = timeEvent
                      }) .
        runGameNetwork .
        flip evalStateT gen .
        runRandomNetwork $
        action
      let
        tickE = outputRenderTick output
        imageB = outputImage output
        soundEffectsE = outputSounds output
      RB.reactimate $ doRendering <$> (imageB <@ tickE)
      RB.reactimate $ mapM_ (playSoundEffect server) <$> soundEffectsE
    doRendering image = do
      clear renderer
      runRenderer renderer absoluteImage
      present renderer
      where
        absoluteImage :: CompositingNode SDL.Texture
        absoluteImage = fromRelativeCompositor (fromIntegral <$> resolutionLinear) image
    fireIfNotQuit (SDL.Event { eventPayload = ev })
      | ev == QuitEvent = return True
      | otherwise = (snd sdlHandler) ev >> return False
    eventLoop = do
      now <- ticks
      let go t0 = do
            sdlEvents <- pollEvents
            abort <- or <$> mapM fireIfNotQuit sdlEvents
            t1 <- ticks
            (snd timeHandler) (t1 - t0)
            if abort then return () else go t1
      go now
  program <- compile network
  actuate program
  eventLoop

reactimate :: RB.Event (IO ()) -> Game ()
reactimate = lift . lift . RB.reactimate

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

buttonPressEvent
  desiredScancode
  (KeyboardEventData { keyboardEventRepeat = is_repeat
                     , keyboardEventKeysym = keysym
                     , keyboardEventKeyMotion = keymotion
                     })
  | (keysymScancode keysym) /= desiredScancode = Nothing
  | is_repeat = Nothing
  | otherwise = Just (keymotion == Pressed)

unionsWith _ [] = never
unionsWith _ [x] = x
unionsWith fun (x:xs) = unionWith fun x (unionsWith fun xs)

filterRepeats ev = do
  (maybeEvents, _) <- mapAccum Nothing $
    (\ x ->
       maybe
       (Just x, Just x)
       (\ y -> if x == y
               then (Nothing, Just y)
               else (Just x, Just x))
    ) <$> ev
  return $ filterJust maybeEvents

-- | generateTicks takes as its argument the desired interval in micro
-- seconds.  60 FPS would be something like
--
-- > sixtyFPS <- generateTicks (pure 16666)
generateTicks :: (Integral i) => Behavior i -> Game (RB.Event ())
generateTicks deltaB = do
  timeE <- lift . GameNetwork $ asks inputTimeEvents
  lift . fmap (filterJust . fst) $
    mapAccum
    0
    ( (\ (delta,tx') accu' ->
         let
           tx = fromIntegral tx' * 1000
           accu = accu' + tx
         in
           if accu > delta
           then (Just (), accu - delta)
           else (Nothing, accu)
      ) <$> ((,) <$> deltaB <@> timeE)
    )

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

data Tidings a = Tidings { rumors :: RB.Event a
                         , facts :: Behavior a
                         }

tidings = Tidings

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

instance Monoid a => Monoid (Tidings a) where
  mempty = tidings never (pure mempty)
  mappend t1 t2 =
    mappend <$> t1 <*> t2

instance Monoid a => Monoid (Behavior a) where
  mappend a b = mappend <$> a <*> b
  mempty = pure mempty

instance Monoid a => Monoid (RB.Event a) where
  mappend a b = unionWith mappend a b
  mempty = never
