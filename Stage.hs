{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module Stage ( Stage
             , stageAssetCache
             , stageData
             , loadStage
             , emptyStage
             )
where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Array
import           Data.List
import qualified Data.Map.Lazy as MapL
import           Data.Tiled hiding (Image)
import           Data.Vector as Vector ((!?))
import           Data.Word
import           GHC.Generics
import qualified SDL
import           System.Directory

import           Paths_geimskell
import           TileSet

type ArrayIx = (Int,Int)

type GId = Word32

data Stage = Stage { stageWidth :: Int
                   , stageHeight :: Int
                   , stageData :: Array (Int,Int) (Maybe GameTile)
                   , stageAssetCache :: AssetCache
                   }
           deriving Generic

instance NFData Stage where
  rnf stage = stageWidth stage `seq`
              stageHeight stage `seq`
              rnf (stageData stage) `seq`
              ()

printExceptions :: IO (Maybe a) -> IO (Maybe a)
printExceptions action = catch action
  (\ e -> do
      putStrLn "An error occured"
      print (e :: SomeException)
      return Nothing
  )

loadStage :: SDL.Renderer -> IO (Maybe Stage)
loadStage renderer =
  getDataFileName "assets/stages" >>=
  flip withCurrentDirectory (printExceptions run)
  where
    makeStage cache action = return . Just $ action cache
    printErrorMessage msg = do
      putStrLn msg
      return Nothing
    stagePath = "stage.tmx"
    run = do
      (eitherMakeStage, cache) <- runGraphicsLoading emptyAssetCache $ do
        printMsg $ "Start loading map @ "++stagePath
        tiledMap <- liftIO $ loadMapFile stagePath
        tiledToStage renderer tiledMap
      either printErrorMessage (makeStage cache) eitherMakeStage

tiledToStage :: SDL.Renderer
             -> TiledMap
             -> GraphicsLoading (AssetCache -> Stage)
tiledToStage renderer tiledMap = do
  let
    stageWidth = mapWidth tiledMap
    stageHeight = mapHeight tiledMap
    arrayBounds = ( (0,0), (stageWidth - 1, stageHeight - 1) )
  tileLayer <-
    liftMaybe "Could not find tile layer in map" mTileLayer
  printMsg $ "Load tile layer "++layerName tileLayer
  layerContents <-
    liftMaybe "Could not load tile information from layer" $
    case layerContents tileLayer of
      LayerContentsTiles tileData -> Just tileData
      _ -> Nothing
  tileMapping <- tileLookupMap renderer tiledMap
  printMsg $ rnf tileMapping `seq` "Tile Map loaded"
  let
    tiles = texturesFromTileData arrayBounds tileMapping layerContents
  printMsg $ rnf tiles `seq` "Tile Information Loaded"
  let
    stageData = array arrayBounds tiles
  return (\ stageAssetCache -> Stage {..})
  where
    layers = mapLayers tiledMap
    mTileLayer = find isTileLayer layers

texturesFromTileData :: (ArrayIx,ArrayIx)
                     -> MapL.Map GId GameTile
                     -> TileData
                     -> [(ArrayIx,Maybe GameTile)]
texturesFromTileData
  ((minX,minY),(maxX,maxY)) tileMapping tileData = do
  x <- [minX .. maxX]
  y <- [minY .. maxY]
  let mGid =
        tileData !? y >>= (!? x) >>= fmap tileIndexGid
  return $ ( (x,y),
             mGid >>=
             flip MapL.lookup tileMapping
           )

isTileLayer :: Layer -> Bool
isTileLayer (Layer {layerContents = (LayerContentsTiles _)}) = True
isTileLayer _ = False

emptyStage :: Stage
emptyStage =
  Stage
  { stageWidth = 0
  , stageHeight = 0
  , stageData = listArray ((0,0), (0,0)) []
  , stageAssetCache = emptyAssetCache
  }
