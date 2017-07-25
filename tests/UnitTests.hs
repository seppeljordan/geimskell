module Main where

import Test.QuickCheck
import Test.Hspec
import TileSet
import Data.Tiled.Types

main = hspec $ do
  describe "TileSet.tilesetColumns" $ do
    it "finds correctly out column number when no spacing/margin" $
      property $ \ (Positive dims) (Positive n) ->
      let
        img = Image { iSource = ""
                    , iTrans = Nothing
                    , iWidth = n * dims
                    , iHeight = n * dims
                    }
        ts = Tileset { tsName = "ts"
                     , tsInitialGid = fromIntegral 1
                     , tsTileWidth = dims
                     , tsTileHeight = dims
                     , tsTileCount = n*n
                     , tsSpacing = 0
                     , tsMargin = 0
                     , tsImages = [img]
                     , tsProperties = []
                     , tsTiles =
                       [
                         Tile { tileId = fromIntegral $ x * n + y
                              , tileProperties = []
                              , tileImage = Nothing
                              , tileObjectGroup = []
                              , tileAnimation = Nothing
                              }
                       | x <- [0..(n-1)]
                       , y <- [0..(n-1)]
                       ]
                     }
      in tilesetColumns ts == n
