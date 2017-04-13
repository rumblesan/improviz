module Gfx.GeometryBuffers where

import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr

import Graphics.Rendering.OpenGL

import Gfx.Geometries

data VBO = VBO VertexArrayObject ArrayIndex NumArrayIndices deriving (Show, Eq)

data GeometryBuffers = GeometryBuffers {
    cubeBuffer :: VBO,
    cubeWireBuffer :: VBO,
    rectBuffer :: VBO,
    rectWireBuffer :: VBO,
    cylinderBuffer :: VBO,
    cylinderWireBuffer :: VBO,
    lineBuffer :: VBO
  } deriving (Show, Eq)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

createBuffer :: [Vertex3 GLfloat] -> IO VBO
createBuffer verts = do
  vbo <- genObjectName
  bindVertexArrayObject $= Just vbo
  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  let
    firstIndex = 0
    vPosition = AttribLocation 0
    numVertices = length verts
    vertexSize = sizeOf (head verts)
    size = fromIntegral (numVertices * vertexSize)
  withArray verts $ \ptr ->
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)
  vertexAttribPointer vPosition $= (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset firstIndex))
  vertexAttribArray vPosition $= Enabled
  return $ VBO vbo firstIndex (fromIntegral numVertices)

createAllBuffers :: IO GeometryBuffers
createAllBuffers = let
    cb = createBuffer $ triVertexArray (cubeVertices 0.2) cubeTriangles
    cwb = createBuffer $ lineVertexArray (cubeVertices 0.2) cubeWireframe
    rb = createBuffer $ triVertexArray (rectVertices 0.2) rectTriangles
    rwb = createBuffer $ lineVertexArray (rectVertices 0.2) rectWireframe
    lwb = createBuffer $ lineVertexArray (lineVertices 0.2) lineWireframe
    cyb = createBuffer $ triVertexArray (cylinderVertices 0.2 0.2 8) $ cylinderTriangles 8
    cywb = createBuffer $ lineVertexArray (cylinderVertices 0.2 0.2 8) $ cylinderWireframe 8
  in
    GeometryBuffers <$> cb <*> cwb <*> rb <*> rwb <*> cyb <*> cywb <*> lwb
