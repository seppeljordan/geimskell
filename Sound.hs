{-# LANGUAGE RecordWildCards #-}

module Sound where

import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad.STM
import System.Process
import Control.Concurrent

data SoundEffect = SoundShoot

sendCommand server action =
  atomically $ writeTChan (serverCommandChannel server) action

playSoundEffect server SoundShoot = sendCommand server (callCommand "csound -odac osc_send.csd")

withSoundServer action = do
  server <- createServer
  action server `finally` destroyServer server

data SoundServer =
  SoundServer { serverCommandChannel :: TChan (IO ())
              , serverThreadId :: ThreadId
              }

createServer = do
  serverCommandChannel <- newTChanIO
  serverThreadId <- forkIO $ runServer serverCommandChannel
  return SoundServer {..}

destroyServer = killThread . serverThreadId

runServer chan =
  go
  where
    go = do
      cmd <- atomically $ readTChan chan
      _ <- cmd
      go
