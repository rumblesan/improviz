module Gfx.VAO
  ( VAO(..)
  , create
  , delete
  , bind
  )
where

import           Control.Monad                  ( forM_ )
import           Foreign.Ptr                    ( nullPtr )

import qualified Graphics.Rendering.OpenGL     as GL
import           Graphics.Rendering.OpenGL      ( ($=) )

import qualified Gfx.VertexDataBuffer          as VDB

data VAO = VAO { vertexArrayObject :: GL.VertexArrayObject
               , buffers :: [(GL.AttribLocation, VDB.VertexDataBuffer)]
               } deriving (Show, Eq)

create :: [(GL.AttribLocation, VDB.VertexDataBuffer)] -> IO VAO
create buffers = do
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  forM_ buffers bindVDB
  return $ VAO vao buffers

bindVDB :: (GL.AttribLocation, VDB.VertexDataBuffer) -> IO ()
bindVDB (attribLoc, vdb) = do
  GL.bindBuffer GL.ArrayBuffer $= Just (VDB.buffer vdb)
  GL.vertexAttribPointer attribLoc
    $= ( GL.ToFloat
       , GL.VertexArrayDescriptor (VDB.vertexComponents vdb) GL.Float 0 nullPtr
       )
  GL.vertexAttribArray attribLoc $= GL.Enabled

delete :: VAO -> IO ()
delete vao = do
  forM_ (buffers vao) (VDB.delete . snd)
  GL.deleteObjectName $ vertexArrayObject vao

bind :: VAO -> IO ()
bind vao = GL.bindVertexArrayObject $= Just (vertexArrayObject vao)
