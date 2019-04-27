module Enemy where

import Reactive.Banana
import System.Random

import Reactive
import Geometry
import Random

newtype Enemy = Enemy { fromEnemy :: Rectangle }

enemySpeed = 0.6 -- per second

makeEnemies :: Behavior Number
            -> Event ()
            -> Event Int
            -> Game (Event ([Enemy] -> [Enemy]))
makeEnemies spawnXPosition spawnTicksE timeDeltaE = do
  let
    randomVector :: StdGen -> (Vector, StdGen)
    randomVector =
      runRandom $
      makeVector 1 <$>
      randR (-0.5,0.5)
  let moveVectorE =
        (\ xpos vec -> vectorAdd (makeVector xpos 0) vec) <$>
        spawnXPosition
  randomPosition <-
    randomGenerator
    (randomVector <$ spawnTicksE)
  let
    changeEnemies = unionWith (.)
      ((\ point es -> pointToEnemy point : es) <$> apply moveVectorE randomPosition)
      (moveEnemies <$> timeDeltaE)
    moveEnemies = map . moveEnemy
    moveEnemy timeDelta =
      let
        moveDistance :: Number
        moveDistance = enemySpeed * 0.001 * 0.001 * fromIntegral timeDelta
      in
        Enemy .
        flip translateRectangle (makeVector (- moveDistance) 0) .
        fromEnemy
    pointToEnemy v =
      let x = pointX v
          y = pointY v
      in Enemy $ makeRectangle
         (makeVector (x - 0.1) (y - 0.1))
         (makeVector (x + 0.1) (y + 0.1))
  return $ changeEnemies
