module Shoot where

import Reactive.Banana

import Geometry
import Reactive

data Projectile = Projectile { projectileRect :: Rectangle }
  deriving (Show,Read,Eq,Ord)

translateProjectile v p =
  p { projectileRect = translateRectangle (projectileRect p) v }

projectilePosition = rectangleMidpoint . projectileRect

makeShoot :: Behavior Vector
          -> Behavior (Number,Number)
          -> Event ()
          -> Behavior [Projectile]
          -> Event Tick
          -> Tidings [Projectile]
makeShoot positionB dimensionsB triggerE shotsB tickE =
  tidings shotsE shotsB
  where
    shotsE :: Event [Projectile]
    shotsE =
      flip ($) <$> shotsB <@>
      unionWith (.) newProjectiles projectilesMove
    newProjectiles :: Event ([Projectile] -> [Projectile])
    newProjectiles =
      (\ v (width,height) ->
         let rect = makeRectangle
                    (makeVector (x - width/2) (y - height/2))
                    (makeVector (x + width/2) (y + height/2))
             x = pointX v
             y = pointY v
         in (Projectile rect:)) <$>
      positionB <*> dimensionsB <@ triggerE
    projectilesMove :: Event ([Projectile] -> [Projectile])
    projectilesMove =
      map (translateProjectile (makeVector 0.032 0)) <$ tickE
