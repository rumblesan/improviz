module Gfx.VertexDataBuffer
  ( VertexDataBuffer(..)
  , create
  , delete
  )
where

import           Data.Maybe                     ( listToMaybe )
import           Foreign.Marshal.Array          ( withArray )
import           Foreign.Storable               ( Storable
                                                , sizeOf
                                                )

import           Graphics.Rendering.OpenGL     as GL

data VertexDataBuffer = VertexDataBuffer { buffer :: GL.BufferObject
                                         , bufferLength :: GLsizei
                                         , elementSize :: GLsizeiptr
                                         } deriving (Show, Eq)

create :: Storable a => [a] -> IO VertexDataBuffer
create verts =
  let numVertices = fromIntegral $ length verts
      vertexSize  = fromIntegral $ maybe 0 sizeOf (listToMaybe verts)
      elementSize = fromIntegral (numVertices * vertexSize)
  in  do
        arrayBuffer <- GL.genObjectName
        GL.bindBuffer ArrayBuffer $= Just arrayBuffer
        withArray verts $ \ptr ->
          GL.bufferData ArrayBuffer $= (elementSize, ptr, StaticDraw)
        GL.bindBuffer ArrayBuffer $= Nothing
        return $ VertexDataBuffer arrayBuffer numVertices elementSize

delete :: VertexDataBuffer -> IO ()
delete vdb = GL.deleteObjectName $ buffer vdb
