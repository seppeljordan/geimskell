module Geimskell.Render where

import           Data.Array
import           Enemy
import           Geimskell.WorldState
import           Geometry
import qualified Linear.V2 as L
import           Reactive
import           SDL.Compositor
import           SDL.Compositor.ResIndependent
import           Shoot
import           Spaceship
import           Stage
import           TileSet

red = rgba 255 0 0 255
blue = rgba 0 0 255 255
green = rgba 0 255 0 255

renderRectangle :: Color -> Rectangle -> ResIndependentImage
renderRectangle col rect =
  translateR offset (filledRectangleR (L.V2 width height) col)
  where
    width = abs $ (pointX . rectangleA $ rect) - (pointX . rectangleB $ rect)
    height = abs $ (pointY . rectangleA $ rect) - (pointY . rectangleB $ rect)
    offset =
      L.V2
      (avg (pointX . rectangleA $ rect) (pointX . rectangleB $ rect))
      (avg (pointY . rectangleA $ rect) (pointY . rectangleB $ rect))
    avg a b = (a+b)/2

class HasResolutionIndependentImage a where
  renderResolutionIndependentImage :: Number -> a -> ResIndependentImage

instance (HasResolutionIndependentImage a) => HasResolutionIndependentImage [a] where
  renderResolutionIndependentImage cameraPosition =
    mconcat . map (renderResolutionIndependentImage cameraPosition)

class HasImage a where
  renderImage :: Number -> a -> Image

instance (HasImage a) => HasImage [a] where
  renderImage cameraPosition = mconcat . map (renderImage cameraPosition)

adjustToCameraR :: Number -> ResIndependentImage -> ResIndependentImage
adjustToCameraR cameraPosition =
  translateR (L.V2 (-cameraPosition) 0) .
  translateR (L.V2 0.5 0.5) .
  flipC (L.V2 False True)

fromResolutionIndependent :: (HasResolutionIndependentImage a)
                          => Number -> a -> Image
fromResolutionIndependent cameraPosition x =
  fromRelativeCompositor
  (fromIntegral <$> L.V2 screenWidth screenHeight)
  (renderResolutionIndependentImage cameraPosition x)

instance HasImage Stage where
  renderImage camPosition stage =
    mconcat . fmap makeImage . filter onScreen . assocs $ (stageData stage)
    where
      camPositionAbsolute = floor $ camPosition * 600
      tileWidth = 32 :: Int
      tileHeight = 32 :: Int
      relativeWidth = 1/fromIntegral tileWidth :: Number
      makeImage (_,Nothing) = mempty
      makeImage ((x,y), Just tile) =
        ( translateA
          ( L.V2
            (x * tileWidth - camPositionAbsolute)
            (y * tileHeight + tileHeight `div` 2)
          )
        ) $
        sizedA (L.V2 tileWidth tileHeight) (tileTexture tile)
      onScreen ((gridXPos,_),_) =
        -- gridXPos is just the position in the tilegrid and has nothing
        -- to do with the actual position on the screen
        x * relativeWidth > camPosition - 2 &&
        x * relativeWidth < camPosition + 2
        where
          x = fromIntegral gridXPos

instance HasResolutionIndependentImage PlayerShip where
  renderResolutionIndependentImage xPosition playerShip =
    adjustToCameraR xPosition .
    renderRectangle blue $
    playerShipRectangle playerShip

instance HasImage PlayerShip where
  renderImage = fromResolutionIndependent

instance HasResolutionIndependentImage Projectile where
  renderResolutionIndependentImage cameraPosition =
    adjustToCameraR cameraPosition . renderRectangle red . projectileRect

instance HasImage Projectile where
  renderImage = fromResolutionIndependent

instance HasResolutionIndependentImage Enemy where
  renderResolutionIndependentImage camPosition =
    adjustToCameraR camPosition . renderRectangle green . fromEnemy

instance HasImage Enemy where
  renderImage = fromResolutionIndependent

instance HasResolutionIndependentImage WorldState where
  renderResolutionIndependentImage xPosition worldstate = outputImage
    where
      outputImage =
        (renderResolutionIndependentImage xPosition . wsEnemies $ worldstate) <>
        (renderResolutionIndependentImage xPosition . wsProjectiles $ worldstate) <>
        (renderResolutionIndependentImage xPosition . wsPlayer $ worldstate)

instance HasImage WorldState where
  renderImage = fromResolutionIndependent
