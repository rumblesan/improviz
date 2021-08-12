module Gfx.Shaders
  ( getUniformLoc
  , getAttribLoc
  ) where

import qualified Graphics.Rendering.OpenGL     as GL

getUniformLoc
  :: GL.Program
  -> (GL.GLint, GL.VariableType, String)
  -> IO (String, GL.VariableType, GL.UniformLocation)
getUniformLoc p (_, vt, uname) = do
  ul <- GL.get $ GL.uniformLocation p uname
  return (uname, vt, ul)

getAttribLoc
  :: GL.Program
  -> (GL.GLint, GL.VariableType, String)
  -> IO (String, GL.VariableType, GL.AttribLocation)
getAttribLoc p (_, vt, aname) = do
  al <- GL.get $ GL.attribLocation p aname
  return (aname, vt, al)

