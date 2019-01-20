module Gfx.OpenGL where

import           Gfx.Types                 (Colour (..))
import           Graphics.Rendering.OpenGL (Color4 (..), GLfloat)

colToGLCol :: Colour -> Color4 GLfloat
colToGLCol (Colour r g b a) = Color4 r g b a
