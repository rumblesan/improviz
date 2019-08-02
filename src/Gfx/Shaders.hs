{-# LANGUAGE TemplateHaskell #-}

module Gfx.Shaders where

import qualified Graphics.GL                   as GLRaw
import           Graphics.Rendering.OpenGL     as GL

import           Foreign.Marshal.Utils
import           Foreign.Ptr

import           Linear.Matrix                  ( M44 )

import           Data.FileEmbed                 ( embedFile )

import           Gfx.LoadShaders
import           Gfx.OpenGL                     ( colToGLCol )
import           Gfx.Types                      ( Colour )

data Shaders = Shaders
  { shaderProgram    :: GL.Program
  , mvpMatrixUniform :: GL.UniformLocation
  , colourUniform    :: GL.UniformLocation
  } deriving (Show, Eq)

createColourShaders :: IO Shaders
createColourShaders = do
  program <- loadShaders
    [ ShaderInfo
      VertexShader
      (ByteStringSource $(embedFile "src/assets/shaders/basic.vert"))
    , ShaderInfo
      FragmentShader
      (ByteStringSource $(embedFile "src/assets/shaders/basic.frag"))
    ]
  GL.currentProgram $= Just program
  mvpMatUniform <- GL.get $ uniformLocation program "MVPMat"
  colourU       <- GL.get $ uniformLocation program "vertexColor"
  return $ Shaders program mvpMatUniform colourU

createTextureShaders :: IO Shaders
createTextureShaders = do
  program <- loadShaders
    [ ShaderInfo
      VertexShader
      (ByteStringSource $(embedFile "src/assets/shaders/texture.vert"))
    , ShaderInfo
      FragmentShader
      (ByteStringSource $(embedFile "src/assets/shaders/texture.frag"))
    ]
  GL.currentProgram $= Just program
  mvpMatUniform <- GL.get $ uniformLocation program "MVPMat"
  return $ Shaders program mvpMatUniform mvpMatUniform

createStrokeShaders :: IO Shaders
createStrokeShaders = do
  program <- loadShaders
    [ ShaderInfo
      VertexShader
      (ByteStringSource $(embedFile "src/assets/shaders/stroke.vert"))
    , ShaderInfo
      FragmentShader
      (ByteStringSource $(embedFile "src/assets/shaders/stroke.frag"))
    ]
  GL.currentProgram $= Just program
  mvpMatUniform <- GL.get $ uniformLocation program "MVPMat"
  colourU       <- GL.get $ uniformLocation program "vertexColor"
  return $ Shaders program mvpMatUniform colourU

setMVPMatrixUniform :: Shaders -> M44 GLfloat -> IO ()
setMVPMatrixUniform (Shaders _ (UniformLocation mvpMatUniform) _) mvpMat =
  with mvpMat
    $ GLRaw.glUniformMatrix4fv mvpMatUniform 1 (fromBool True)
    . castPtr

setColourUniform :: Shaders -> Colour -> IO ()
setColourUniform shaders c = uniform (colourUniform shaders) $= colToGLCol c
