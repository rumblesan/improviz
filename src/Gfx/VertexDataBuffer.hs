module Gfx.VertexDataBuffer
  ( VertexDataBuffer(..)
  , create
  , delete
  ) where

import           Data.Maybe                     ( listToMaybe )
import           Foreign.Marshal.Array          ( withArray )
import           Foreign.Storable               ( Storable
                                                , sizeOf
                                                )

import           Graphics.Rendering.OpenGL     as GL

data VertexDataBuffer = VertexDataBuffer
  { buffer           :: GL.BufferObject
  , vertexCount      :: GLsizei
  , vertexComponents :: NumComponents
  }
  deriving (Show, Eq)

create :: Storable a => [a] -> NumComponents -> IO VertexDataBuffer
create verts vertComponentCount =
  let vCount     = fromIntegral $ length verts
      vSize      = fromIntegral $ maybe 0 sizeOf (listToMaybe verts)
      bufferSize = fromIntegral (vCount * vSize)
  in  do
        arrayBuffer <- GL.genObjectName
        GL.bindBuffer ArrayBuffer $= Just arrayBuffer
        withArray verts
          $ \ptr -> GL.bufferData ArrayBuffer $= (bufferSize, ptr, StaticDraw)
        GL.bindBuffer ArrayBuffer $= Nothing
        return $ VertexDataBuffer arrayBuffer vCount vertComponentCount

delete :: VertexDataBuffer -> IO ()
delete vdb = GL.deleteObjectName $ buffer vdb
