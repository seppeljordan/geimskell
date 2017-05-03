module Enemy where

import Reactive.Banana
import System.Random

import Reactive
import Geometry
import Random

type Enemy = Rectangle

makeEnemies :: Behavior [Enemy]
            -> Event ()
            -> Game (Tidings [Enemy])
makeEnemies enemiesB spawnTicksE = do
  let
    randomVector :: StdGen -> (Vector, StdGen)
    randomVector =
      runRandom (makeVector <$> randR (-0.5,0.5) <*> randR (-0.5,0.5))
  randomPosition <- randomGenerator (randomVector <$ spawnTicksE)
  let
    enemiesE =
      flip (:) <$>
      enemiesB <@>
      fmap pointToEnemy randomPosition
    pointToEnemy v =
      let x = pointX v
          y = pointY v
      in makeRectangle
         (makeVector (x - 0.1) (y - 0.1))
         (makeVector (x + 0.1) (y + 0.1))
  return $ tidings enemiesE enemiesB
