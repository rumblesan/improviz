module Gfx.VertexIndexBuffer
  ( VertexIndexBuffer(..)
  , create
  , delete
  ) where

import           Data.Maybe                     ( listToMaybe )
import           Foreign.Marshal.Array          ( withArray )
import           Foreign.Storable               ( Storable
                                                , sizeOf
                                                )

import           Graphics.Rendering.OpenGL     as GL

data VertexIndexBuffer = VertexIndexBuffer
  { buffer     :: GL.BufferObject
  , indexCount :: GLsizei
  }
  deriving (Show, Eq)

create :: Storable a => [a] -> IO VertexIndexBuffer
create indices =
  let
    vCount     = fromIntegral $ length indices
    vSize      = fromIntegral $ maybe 0 sizeOf (listToMaybe indices)
    bufferSize = fromIntegral (vCount * vSize)
  in
    do
      arrayBuffer <- GL.genObjectName
      GL.bindBuffer ElementArrayBuffer $= Just arrayBuffer
      withArray indices $ \ptr ->
        GL.bufferData ElementArrayBuffer $= (bufferSize, ptr, StaticDraw)
      GL.bindBuffer ElementArrayBuffer $= Nothing
      return $ VertexIndexBuffer arrayBuffer vCount

delete :: VertexIndexBuffer -> IO ()
delete vdb = GL.deleteObjectName $ buffer vdb
