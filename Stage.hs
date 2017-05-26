{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Stage where

import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Array
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.StateVar hiding (get)
import           Data.Tiled hiding (Image)
import qualified SDL
import qualified SDL.Image as SDL
import           System.Directory

data Stage = Stage { stageWidth :: Int
                   , stageHeight :: Int
                   , stageData :: Array (Int,Int) (Maybe SDL.Texture)
                   , stageAssetCache :: AssetCache
                   }

data AssetCache =
  AssetCache { acSpriteMaps :: M.Map String SDL.Texture
             , acSprites :: M.Map TextureKey SDL.Texture
             }

emptyAssetCache = AssetCache {..}
  where
    acSprites = mempty
    acSpriteMaps = mempty

destroyAssetCache cache =
  mapM_ SDL.destroyTexture (acSprites cache) >>
  mapM_ SDL.destroyTexture (acSpriteMaps cache)

data TextureKey = TextureKey { tkPath :: FilePath
                             , tkOffset :: (Int,Int)
                             , tkDims :: (Int,Int)
                             }
  deriving (Show,Read,Eq,Ord)

type TextureCache = M.Map TextureKey SDL.Texture

loadStage :: SDL.Renderer -> IO (Maybe Stage)
loadStage renderer =
  withCurrentDirectory "assets/stages" . runMaybeT $
  tiledToStage renderer =<< liftIO (loadMapFile stagePath)
  where
    stagePath = "stage.tmx"

tiledToStage :: SDL.Renderer -> TiledMap -> MaybeT IO Stage
tiledToStage renderer tiledMap = do
  (stageData,stageAssetCache) <- MaybeT $ do
    (maybeData,cache) <- flip runStateT emptyAssetCache . runMaybeT $ generateStage
    case maybeData of
      Nothing -> return Nothing
      Just tileData -> return . Just $ (tileData,cache)
  return $ Stage {..}
  where
    stageWidth = mapWidth tiledMap
    stageHeight = mapHeight tiledMap
    generateStage = do
      tileLayer <- MaybeT . return $ getFirstTileLayer tiledMap
      let
        tiles = [ ((x,y), M.lookup (x,y) (layerData tileLayer))
                | x <- [0..stageWidth-1]
                , y <- [0..stageHeight-1]
                ]
      textures <-
        mapM (\ (c,t) ->
                maybe
               (return (c,Nothing))
               (fmap (c,) . lift . loadTileFromTileset renderer (mapTilesets tiledMap))
               t
             ) tiles
      return $ array ((0,0),(stageWidth-1,stageHeight-1)) textures

getFirstTileLayer :: TiledMap -> Maybe Layer
getFirstTileLayer = find isTileLayer . mapLayers

isTileLayer (Layer _ _ _ _ _) = True
isTileLayer _ = False

listOfTiles :: ((Int,Int),(Int,Int)) -> Layer -> [((Int,Int),Tile)]
listOfTiles ((minX,minY),(maxX,maxY)) (Layer {layerData = tiles}) =
  foldl
  (\ a (c,mt) ->
     case mt of
       Just t -> (c,t):a
       Nothing -> a)
  []
  [ ((x,y), M.lookup (x,y) tiles) | x <- [minX .. maxX]
                                  , y <- [minY .. maxY]
                                  ]
  where
    getTileAtPosition pos = M.lookup pos tiles
listOfTiles _ _ = []

loadTextureThroughCache :: SDL.Renderer
                        -> FilePath
                        -> StateT AssetCache IO SDL.Texture
loadTextureThroughCache renderer path = do
  cache <- get
  case M.lookup path (acSpriteMaps cache) of
    Nothing -> do
      tex <- liftIO $ SDL.loadTexture renderer path
      put
        cache
        { acSpriteMaps = (M.insert path tex (acSpriteMaps cache))}
      return tex
    Just tex -> return tex

loadTileFromTileset :: SDL.Renderer
                    -> [Tileset]
                    -> Tile
                    -> StateT AssetCache IO (Maybe SDL.Texture)
loadTileFromTileset renderer tilesets
    (Tile gid verticalFlip horizontalFlip diagonalFlip) = runMaybeT $ do
  tileset <- MaybeT . return $ findTileset
  image <- MaybeT . return $ tsImages tileset `safeIndex` 0
  tkOffset <- MaybeT . return $ spriteOffset tileset
  let
    key = TextureKey {..}
    tkDims = ( fromIntegral . tsTileWidth $ tileset
             , fromIntegral . tsTileHeight $ tileset
             )
    tkPath = iSource image
  lift $ loadTileTexture renderer key
  where
    findTileset =
        find
        (\ ts ->
            tsInitialGid ts <= gid &&
            gid <= tsInitialGid ts + (fromIntegral . length . tsImages $ ts))
        tilesets
    spriteOffset tileset = do
      image <- (tsImages tileset) `safeIndex` 0
      let
        tilePerRow = iWidth image `div` tsTileWidth tileset
        relativeGid = fromIntegral $ gid - tsInitialGid tileset
        x = relativeGid `mod` tilePerRow
        y = relativeGid `div` tilePerRow
      Just $ ( x * fromIntegral (tsTileWidth tileset)
             , y * fromIntegral (tsTileHeight tileset)
             )

loadTileTexture renderer key = do
  tilesetTexture <- loadTextureThroughCache renderer (tkPath key)
  cache <- get
  case M.lookup key $ acSprites cache of
    Just tex -> return tex
    Nothing -> do
      tileTex <- liftIO $ SDL.createTexture
        renderer
        SDL.ARGB8888
        SDL.TextureAccessTarget
        (fromIntegral <$> SDL.V2 (fst . tkDims $ key) (snd . tkDims $ key))
      liftIO $ SDL.rendererRenderTarget renderer $= Just tileTex
      liftIO $ SDL.copy
        renderer
        tilesetTexture
        (Just sourceRect)
        Nothing
      liftIO $ SDL.rendererRenderTarget renderer $= Nothing
      put $ cache { acSprites = M.insert key tileTex $ acSprites cache}
      return tileTex
  where
    sourceRect = fromIntegral <$>
      SDL.Rectangle
      (SDL.P
        (SDL.V2 (fst . tkOffset $ key) (snd . tkOffset $ key)))
      (SDL.V2 (fst . tkDims $ key) (snd . tkOffset $ key))

safeIndex :: [a] -> Int -> Maybe a
safeIndex (x:xs) n
  | n < 0 = Nothing
  | n == 0 = Just x
  | n > 0 = safeIndex xs (n - 1)
safeIndex [] _ = Nothing
