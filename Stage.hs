{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module Stage where

import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Data.Array
import           Data.List
import qualified Data.Map.Lazy as MapL
import           Data.Maybe
import           Data.Tiled hiding (Image)
import           Data.Vector as Vector ((!?))
import           Data.Word
import           GHC.Generics
import qualified SDL
import           System.Directory

import           Paths_geimskell
import           TileSet

import           Debug.Trace

traceMap f x = trace (show $ f x) x

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

loadStage :: SDL.Renderer -> IO (Maybe Stage)
loadStage renderer = do
  assetDir <- getDataFileName "assets/stages"
  (eitherMakeStage, cache) <-
    withCurrentDirectory assetDir . runGraphicsLoading emptyAssetCache $ do
      tiledMap <- liftIO $ loadMapFile stagePath
      tiledToStage renderer tiledMap
  case eitherMakeStage of
    Left msg -> do
      putStrLn $ msg
      return Nothing
    Right makeStage -> return . Just . makeStage $ cache
  where
    stagePath = "stage.tmx"

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
  printMsg $ "TileMapping keys: " ++ (show . MapL.keys $ tileMapping)
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

getFirstTileLayer :: TiledMap -> Maybe Layer
getFirstTileLayer = find isTileLayer . mapLayers

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

isTileLayer (Layer {layerContents = (LayerContentsTiles _)}) = True
isTileLayer _ = False

safeIndex :: [a] -> Int -> Maybe a
safeIndex (x:xs) n
  | n < 0 = Nothing
  | n == 0 = Just x
  | n > 0 = safeIndex xs (n - 1)
safeIndex _ _ = Nothing
