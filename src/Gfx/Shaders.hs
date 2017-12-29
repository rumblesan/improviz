{-# LANGUAGE TemplateHaskell #-}

module Gfx.Shaders where

import qualified Graphics.GL               as GLRaw
import           Graphics.Rendering.OpenGL as GL

import           Foreign.Marshal.Utils
import           Foreign.Ptr

import           Data.Vec

import           Data.FileEmbed            (embedFile)

import           Gfx.LoadShaders

data Shaders = Shaders
  { shaderProgram    :: GL.Program
  , mvpMatrixUniform :: GL.UniformLocation
  , colourUniform    :: GL.UniformLocation
  } deriving (Show, Eq)

createColourShaders :: IO Shaders
createColourShaders = do
  program <-
    loadShaders
      [ ShaderInfo
          VertexShader
          (ByteStringSource $(embedFile "assets/shaders/basic.vert"))
      , ShaderInfo
          FragmentShader
          (ByteStringSource $(embedFile "assets/shaders/basic.frag"))
      ]
  GL.currentProgram $= Just program
  mvpMatUniform <- GL.get $ uniformLocation program "MVPMat"
  colourU <- GL.get $ uniformLocation program "vertexColor"
  return $ Shaders program mvpMatUniform colourU

createTextureShaders :: IO Shaders
createTextureShaders = do
  program <-
    loadShaders
      [ ShaderInfo
          VertexShader
          (ByteStringSource $(embedFile "assets/shaders/texture.vert"))
      , ShaderInfo
          FragmentShader
          (ByteStringSource $(embedFile "assets/shaders/texture.frag"))
      ]
  GL.currentProgram $= Just program
  mvpMatUniform <- GL.get $ uniformLocation program "MVPMat"
  return $ Shaders program mvpMatUniform mvpMatUniform

setMVPMatrixUniform :: Shaders -> Mat44 GLfloat -> IO ()
setMVPMatrixUniform (Shaders _ (UniformLocation mvpMatUniform) _) mvpMat =
  with mvpMat $
  GLRaw.glUniformMatrix4fv mvpMatUniform 1 (fromBool True) . castPtr

setColourUniform :: Shaders -> Color4 GLfloat -> IO ()
setColourUniform shaders c = uniform (colourUniform shaders) $= c
