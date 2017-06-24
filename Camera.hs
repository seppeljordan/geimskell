module Camera where

import Reactive.Banana

import Reactive
import Geometry

data Camera = Camera { camPosition :: Number
                     , camSpeed :: Number
                     }

makeCamera :: Event ()
           -> Behavior Camera
           -> Tidings Camera
makeCamera tickE camB =
  tidings
  (moveCamera <$> camB <@ tickE)
  camB
  where
    moveCamera cam =
      cam { camPosition = camPosition cam + camSpeed cam }
