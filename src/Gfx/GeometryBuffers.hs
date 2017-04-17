module Gfx.GeometryBuffers where

import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr

import Graphics.Rendering.OpenGL

import Gfx.Geometries

data VBO = VBO VertexArrayObject BufferObject ArrayIndex NumArrayIndices deriving (Show, Eq)

data GeometryBuffers = GeometryBuffers {
    cubeBuffer :: VBO,
    cubeWireBuffer :: VBO,
    rectBuffer :: VBO,
    rectWireBuffer :: VBO,
    cylinderBuffer :: VBO,
    cylinderWireBuffer :: VBO,
    sphereBuffer :: VBO,
    sphereWireBuffer :: VBO,
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
  return $ VBO vbo arrayBuffer firstIndex (fromIntegral numVertices)

createAllBuffers :: IO GeometryBuffers
createAllBuffers = let
    cb = createBuffer $ triVertexArray (cubeVertices 1) cubeTriangles
    cwb = createBuffer $ lineVertexArray (cubeVertices 1) cubeWireframe
    rb = createBuffer $ triVertexArray (rectVertices 1) rectTriangles
    rwb = createBuffer $ lineVertexArray (rectVertices 1) rectWireframe
    cyb = createBuffer $ triVertexArray (cylinderVertices 1 0.5 8) $ cylinderTriangles 8
    cywb = createBuffer $ lineVertexArray (cylinderVertices 1 0.5 8) $ cylinderWireframe 8
    sb = createBuffer $ triVertexArray (sphereVertices 0.5 12) $ sphereTriangles 12
    swb = createBuffer $ lineVertexArray (sphereVertices 0.5 12) $ sphereWireframe 12
    lwb = createBuffer $ lineVertexArray (lineVertices 1) lineWireframe
  in
    GeometryBuffers <$> cb <*> cwb <*> rb <*> rwb <*> cyb <*> cywb <*> sb <*> swb <*> lwb
