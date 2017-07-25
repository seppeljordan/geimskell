{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module TileSet where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.List
import qualified Data.Map.Lazy as MapL
import qualified Data.Map.Strict as MapS
import           Data.Maybe
import           Data.StateVar hiding (get)
import           Data.Tiled
import           Data.Word
import           GHC.Generics
import qualified SDL
import           SDL.Image as SDL

import Debug.Trace

traceMap f x = trace (show . f $ x) x

data AssetCache =
  AssetCache { acSpriteMaps :: MapS.Map FilePath SDL.Texture
             , acSprites :: MapS.Map TextureKey SDL.Texture
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

type TextureCache = MapS.Map TextureKey SDL.Texture

type GraphicsLoading = ExceptT String (StateT AssetCache IO)

runGraphicsLoading assetCache action =
   flip runStateT assetCache . runExceptT $ action

throwError :: String -> GraphicsLoading a
throwError = throwE

printMsg :: String -> GraphicsLoading ()
printMsg = liftIO . putStrLn

loadTextureThroughCache :: SDL.Renderer
                        -> FilePath
                        -> GraphicsLoading SDL.Texture
loadTextureThroughCache renderer path = do
  cache <- lift get
  case MapS.lookup path (acSpriteMaps cache) of
    Nothing -> do
      printMsg $ "load texture from "++path
      tex <- liftIO $ SDL.loadTexture renderer path
      lift $ put
        cache
        { acSpriteMaps = (MapS.insert path tex (acSpriteMaps cache))}
      return tex
    Just tex -> return tex

loadTextureKey renderer key = do
  tilesetTexture <- loadTextureThroughCache renderer (tkPath key)
  cache <- lift get
  case MapS.lookup key $ acSprites cache of
    Just tex -> return tex
    Nothing -> do
      liftIO . putStrLn $ "load tile "++show key
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
      lift . put $ cache { acSprites = MapS.insert key tileTex $ acSprites cache}
      return tileTex
  where
    sourceRect = fromIntegral <$>
      SDL.Rectangle
      (SDL.P
        (SDL.V2 (fst . tkOffset $ key) (snd . tkOffset $ key)))
      (SDL.V2 (fst . tkDims $ key) (snd . tkDims $ key))

data GameTile = GameTile { tileSolid :: Bool
                         , tileTexture :: SDL.Texture
                         }

instance NFData GameTile where
  rnf tile = tileSolid tile `seq` tile `seq` ()

tileLookupMap renderer =
  foldM addTileSetToMap MapL.empty . mapTilesets . traceMap mapTilesets
  where
    addTileSetToMap tilemap tileset =
      foldM
      (addTileToMap $ tileset)
      tilemap
      (tsTiles tileset)
    addTileToMap tileset tilemap tile = do
      image <- liftMaybe
        ("Tileset "++tsName tileset++" does not contain an image") $
        listToMaybe $ tsImages tileset
      let
        gid = fromIntegral $ tsInitialGid tileset + tileId tile
        localId = fromIntegral $ tileId tile
        columns = (iWidth image - tsMargin tileset + tsSpacing tileset)
                  `div` iWidth image + tsSpacing tileset
        xOffset =
          tsMargin tileset +
          (localId `mod` columns) *
          (tsSpacing tileset + tsTileWidth tileset)
        yOffset = tsMargin tileset +
                  (localId `div` columns) *
                  (tsSpacing tileset + tsTileHeight tileset)
        texturekey = TextureKey { tkPath = iSource image
                                , tkOffset = (xOffset, yOffset)
                                , tkDims = (iWidth image, iHeight image)
                                }
      tex <- loadTextureKey renderer texturekey
      let
        gametile = GameTile { tileTexture = tex
                            , tileSolid = False
                            }
      return $ MapL.insert gid gametile tilemap

liftMaybe _ (Just x) = return x
liftMaybe msg Nothing = throwError msg
