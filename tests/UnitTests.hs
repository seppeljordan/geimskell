{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Maybe
import           Data.Tiled.Types
import           Foreign.C.Types
import           GHC.Word
import           Reactive
import qualified Reactive.Banana as RB
import           Reactive.Banana.Frameworks as RB
import           SDL
import           Stage
import           System.Random
import           Test.Hspec
import           Test.QuickCheck
import           TileSet

data TestEvent a = TestValueEvent a
                 | TestInputEvent EventPayload
                 | TestTimeInputEvent Word32
                 | TestWindowResizeEvent (SDL.V2 CInt)

testEventToMaybeValueEvent (TestValueEvent x) = Just x
testEventToMaybeValueEvent _ = Nothing

testEventToMaybeInputEvent (TestInputEvent x) = Just x
testEventToMaybeInputEvent _ = Nothing

testEventToMaybeTimeInputEvent (TestTimeInputEvent x) = Just x
testEventToMaybeTimeInputEvent _ = Nothing

testEventToMaybeWindowResizeEvent (TestWindowResizeEvent x) = Just x
testEventToMaybeWindowResizeEvent _ = Nothing

interpret :: StdGen
          -> Stage
          -> (RB.Event a -> Game (RB.Event b))
          -> [TestEvent a]
          -> IO [b]
interpret randomGen inputStage networkSetupFunction inputEvents =
  catMaybes <$>
  RB.interpretFrameworks transformationFunction (map Just inputEvents)
  where
    transformationFunction testEvent =
      let
        inputValueEvents = RB.filterJust $
                           testEventToMaybeValueEvent <$> testEvent
        inputSdlEvents = RB.filterJust $
                         testEventToMaybeInputEvent <$> testEvent
        inputTimeEvents = RB.filterJust $
                          testEventToMaybeTimeInputEvent <$> testEvent
        inputWindowSizeEvents = RB.filterJust $
                                testEventToMaybeWindowResizeEvent <$> testEvent
        engineInputs = EngineInputs {..}
        gameNetwork = flip runReaderT engineInputs .
                      runGameNetwork .
                      flip evalStateT randomGen .
                      runRandomNetwork $
                      networkSetupFunction inputValueEvents
      in gameNetwork



main = do
  hspec $ do
    describe "cooldownTimer" $ do
      it "does not trigger before cooldown has passed" $ do
        randomSeed <- newStdGen
        let
          cooldownTime = pure 1000 * 1000 -- 1 second
          network triggerE =
            fst <$> cooldownTimer cooldownTime triggerE
          events = [ TestTimeInputEvent 100
                   , TestValueEvent 1
                   , TestTimeInputEvent 100
                   , TestValueEvent 2
                   , TestTimeInputEvent 800
                   , TestValueEvent 3
                   ]
        interpret randomSeed emptyStage network events >>= flip shouldBe [3]
