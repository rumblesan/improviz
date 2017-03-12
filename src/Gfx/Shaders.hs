module Gfx.Shaders where

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.GL as GLRaw

import Foreign.Marshal.Utils
import Foreign.Ptr

import Data.Vec

import Gfx.LoadShaders

data Shaders = Shaders {
    shaderProgram :: GL.Program
  , mvpMatrixUniform :: GL.UniformLocation
  , colourUniform :: GL.UniformLocation
  } deriving (Show, Eq)

createShaders :: IO Shaders
createShaders =
  do
    program <- loadShaders [
        ShaderInfo VertexShader (FileSource "shaders/triangles.vert"),
        ShaderInfo FragmentShader (FileSource "shaders/triangles.frag")]
    GL.currentProgram $= Just program
    mvpMatUniform <- GL.get $ uniformLocation program "MVPMat"
    colourU <- GL.get $ uniformLocation program "vertexColor"
    return $ Shaders program mvpMatUniform colourU

setMVPMatrixUniform :: Shaders -> Mat44 GLfloat -> IO ()
setMVPMatrixUniform (Shaders _ (UniformLocation mvpMatUniform) _) mvpMat =
  with mvpMat
    $ GLRaw.glUniformMatrix4fv mvpMatUniform 1 (fromBool True)
    . castPtr

setColourUniform :: Shaders -> Color4 GLfloat -> IO ()
setColourUniform shaders c = uniform (colourUniform shaders) $= c
