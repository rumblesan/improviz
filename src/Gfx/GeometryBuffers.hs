module Gfx.GeometryBuffers where

import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable

import           Graphics.Rendering.OpenGL

import           Gfx.Geometries

data VBO =
  VBO VertexArrayObject
      [BufferObject]
      ArrayIndex
      NumArrayIndices
  deriving (Show, Eq)

data GeometryBuffers = GeometryBuffers
  { lineBuffer         :: VBO
  , rectBuffer         :: VBO
  , rectWireBuffer     :: VBO
  , cubeBuffer         :: VBO
  , cubeWireBuffer     :: VBO
  , cylinderBuffer     :: VBO
  , cylinderWireBuffer :: VBO
  , sphereBuffer       :: VBO
  , sphereWireBuffer   :: VBO
  } deriving (Show, Eq)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

createBuffer :: [Vertex3 GLfloat] -> IO VBO
createBuffer verts = do
  vbo <- genObjectName
  bindVertexArrayObject $= Just vbo
  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  let firstIndex = 0
      vPosition = AttribLocation 0
      numVertices = length verts
      vertexSize = sizeOf (head verts)
      size = fromIntegral (numVertices * vertexSize)
  withArray verts $ \ptr -> bufferData ArrayBuffer $= (size, ptr, StaticDraw)
  vertexAttribPointer vPosition $=
    (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset firstIndex))
  vertexAttribArray vPosition $= Enabled
  return $ VBO vbo [arrayBuffer] firstIndex (fromIntegral numVertices)

createBufferWithTexture :: [Vertex3 GLfloat] -> [Vertex2 GLfloat] -> IO VBO
createBufferWithTexture verts textCoords = do
  vbo <- genObjectName
  bindVertexArrayObject $= Just vbo
  vertArrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just vertArrayBuffer
  let firstVIndex = 0
      vPosition = AttribLocation 0
      numVVertices = length verts
      vVertexSize = sizeOf (head verts)
      vsize = fromIntegral (numVVertices * vVertexSize)
  withArray verts $ \ptr -> bufferData ArrayBuffer $= (vsize, ptr, StaticDraw)
  vertexAttribPointer vPosition $=
    (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset firstVIndex))
  vertexAttribArray vPosition $= Enabled
  textArrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just textArrayBuffer
  let firstTIndex = 0
      texcoord = AttribLocation 1
      numTVertices = length textCoords
      tVertexSize = sizeOf (head textCoords)
      tsize = fromIntegral (numTVertices * tVertexSize)
  withArray textCoords $ \ptr ->
    bufferData ArrayBuffer $= (tsize, ptr, StaticDraw)
  vertexAttribPointer texcoord $=
    (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset firstTIndex))
  vertexAttribArray texcoord $= Enabled
  return $
    VBO
      vbo
      [vertArrayBuffer, textArrayBuffer]
      firstVIndex
      (fromIntegral numVVertices)

createAllBuffers :: IO GeometryBuffers
createAllBuffers =
  let lwb = createBuffer $ lineVertexArray (lineVertices 1) lineWireframe
      rb =
        createBufferWithTexture
          (triVertexArray (rectVertices 1) rectTriangles)
          rectTextCoords
      rwb = createBuffer $ lineVertexArray (rectVertices 1) rectWireframe
      cb =
        createBufferWithTexture
          (triVertexArray (cubeVertices 1) cubeTriangles)
          cubeTextCoords
      cwb = createBuffer $ lineVertexArray (cubeVertices 1) cubeWireframe
      cyb =
        createBufferWithTexture
          (triVertexArray (cylinderVertices 1 0.5 8) $ cylinderTriangles 8)
          (triVertexArray cylinderTextCoords $ cylinderTriangles 8)
      cywb =
        createBuffer $
        lineVertexArray (cylinderVertices 1 0.5 8) $ cylinderWireframe 8
      sb =
        createBufferWithTexture
          (triVertexArray (sphereVertices 0.5 12) $ sphereTriangles 12)
          (triVertexArray sphereTextCoords $ sphereTriangles 8)
      swb =
        createBuffer $
        lineVertexArray (sphereVertices 0.5 12) $ sphereWireframe 12
  in GeometryBuffers <$> lwb <*> rb <*> rwb <*> cb <*> cwb <*> cyb <*> cywb <*>
     sb <*>
     swb
