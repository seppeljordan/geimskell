import SDL
import qualified Data.Text as Text

import Stage

getRenderer :: IO Renderer
getRenderer = do
  w <- createWindow (Text.pack "test") defaultWindow
  createRenderer w (-1) defaultRenderer
