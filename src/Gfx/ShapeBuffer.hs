module Gfx.ShapeBuffer
  ( ShapeBuffer(..)
  , create
  , delete
  )
where

import           Foreign.Marshal.Array          ( withArray )
import           Foreign.Storable               ( Storable
                                                , sizeOf
                                                )

import           Graphics.Rendering.OpenGL     as GL

data ShapeBuffer = ShapeBuffer { positionCoordBuffer :: GL.BufferObject
                               , positionCoordLength :: Int
                               , textureCoordBuffer :: GL.BufferObject
                               , textureCoordLength :: Int
                               } deriving (Show, Eq)

create :: [Vertex3 GLfloat] -> [Vertex2 GLfloat] -> IO ShapeBuffer
create posCoords textCoords = do
  posCoordBuffer  <- createVertexBuffer posCoords
  textCoordBuffer <- createVertexBuffer textCoords
  return $ ShapeBuffer { positionCoordBuffer = posCoordBuffer
                       , positionCoordLength = length posCoords
                       , textureCoordBuffer  = textCoordBuffer
                       , textureCoordLength  = length textCoords
                       }

createVertexBuffer :: Storable a => [a] -> IO GL.BufferObject
createVertexBuffer verts =
  let
    numVertices = fromIntegral $ length verts
    vertexSize  = fromIntegral $ sizeOf (head verts)
    size        = fromIntegral (numVertices * vertexSize)
  in
    do
      arrayBuffer <- GL.genObjectName
      GL.bindBuffer ArrayBuffer $= Just arrayBuffer
      withArray verts
        $ \ptr -> GL.bufferData ArrayBuffer $= (size, ptr, StaticDraw)
      GL.bindBuffer ArrayBuffer $= Nothing
      return arrayBuffer

delete :: ShapeBuffer -> IO ()
delete sb = do
  GL.deleteObjectName $ positionCoordBuffer sb
  GL.deleteObjectName $ textureCoordBuffer sb
