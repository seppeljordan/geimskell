module Enemy where

import Reactive.Banana
import System.Random

import Reactive
import Geometry
import Random

type Enemy = Rectangle

makeEnemies :: Behavior Number
            -> Event ()
            -> Event ()
            -> Game (Event ([Enemy] -> [Enemy]))
makeEnemies spawnXPosition spawnTicksE ticksE = do
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
      (moveEnemies <$ ticksE)
    moveEnemies = map moveEnemy
    moveEnemy =
      flip translateRectangle (makeVector (-0.01) 0)
    pointToEnemy v =
      let x = pointX v
          y = pointY v
      in makeRectangle
         (makeVector (x - 0.1) (y - 0.1))
         (makeVector (x + 0.1) (y + 0.1))
  return $ changeEnemies
