module Gfx.VAO
  ( VAO(..)
  , create
  , delete
  , bind
  , draw
  )
where

import           Control.Monad                  ( forM_ )
import           Foreign.Ptr                    ( nullPtr )

import qualified Graphics.Rendering.OpenGL     as GL
import           Graphics.Rendering.OpenGL      ( ($=) )

import qualified Gfx.VertexDataBuffer          as VDB
import           Gfx.VertexIndexBuffer          ( VertexIndexBuffer )
import qualified Gfx.VertexIndexBuffer         as VIB

data VAO = VAO { vertexArrayObject :: GL.VertexArrayObject
               , indexBuffer :: VertexIndexBuffer
               , buffers :: [(GL.AttribLocation, VDB.VertexDataBuffer)]
               } deriving (Show, Eq)

create
  :: VertexIndexBuffer -> [(GL.AttribLocation, VDB.VertexDataBuffer)] -> IO VAO
create indexBuffer buffers = do
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  forM_ buffers bindVDB
  return $ VAO vao indexBuffer buffers

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

draw :: VAO -> IO ()
draw vao =
  let idxBuffer = indexBuffer vao
  in  do
        GL.bindBuffer GL.ElementArrayBuffer $= Just (VIB.buffer idxBuffer)
        GL.drawElements GL.Triangles
                        (VIB.indexCount idxBuffer)
                        GL.UnsignedInt
                        nullPtr

