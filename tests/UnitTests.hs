{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Maybe
import           Foreign.C.Types
import           GHC.Word
import           Reactive
import qualified Reactive.Banana as RB
import           Reactive.Banana.Frameworks as RB
import           SDL
import           Stage
import           System.Random
import           Test.Hspec

data TestEvent a = TestValueEvent a
                 | TestInputEvent EventPayload
                 | TestTimeInputEvent Word32
                 | TestWindowResizeEvent (SDL.V2 CInt)

testEventToMaybeValueEvent :: TestEvent a -> Maybe a
testEventToMaybeValueEvent (TestValueEvent x) = Just x
testEventToMaybeValueEvent _ = Nothing

testEventToMaybeInputEvent :: TestEvent a -> Maybe EventPayload
testEventToMaybeInputEvent (TestInputEvent x) = Just x
testEventToMaybeInputEvent _ = Nothing

testEventToMaybeTimeInputEvent :: TestEvent a -> Maybe Word32
testEventToMaybeTimeInputEvent (TestTimeInputEvent x) = Just x
testEventToMaybeTimeInputEvent _ = Nothing

testEventToMaybeWindowResizeEvent :: TestEvent a -> Maybe (V2 CInt)
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

main :: IO ()
main = do
  hspec $ do
    describe "cooldownTimer" $ do
      it "does not trigger before cooldown has passed" $ do
        randomSeed <- newStdGen
        let
          cooldownTime = pure 1000 * 1000 -- 1 second
          cooldownActiveB = pure True
          network triggerE =
            fst <$> cooldownTimer cooldownActiveB cooldownTime triggerE
          events = [ TestTimeInputEvent 100
                   , TestValueEvent 1
                   , TestTimeInputEvent 100
                   , TestValueEvent 2
                   , TestTimeInputEvent 800
                   , TestValueEvent 3
                   ]
        interpret randomSeed emptyStage network events >>= flip shouldBe ([3] :: [Int])
      it "does not trigger when it is not active" $ do
        randomSeed <- newStdGen
        let
          cooldownTimeB = pure 1000 * 1000
          cooldownActiveB = pure False
          network triggerE =
            fst <$> cooldownTimer cooldownActiveB cooldownTimeB triggerE
          events = [ TestTimeInputEvent 1000
                   , TestValueEvent 1
                   , TestTimeInputEvent 1000
                   , TestValueEvent 2
                   ]
        interpret randomSeed emptyStage network events >>= flip shouldBe ([] :: [Int])
