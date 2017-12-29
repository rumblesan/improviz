module Gfx.GeometryBuffers where

import           Foreign.Marshal.Array     (withArray)
import           Foreign.Storable          (sizeOf)
import           GHC.Int                   (Int32)

import           Graphics.Rendering.OpenGL as GL

import           Gfx.Geometries

import           Gfx.VertexBuffers         (VBO, createVBO, setAttribPointer)

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

createBuffer :: [Vertex3 GLfloat] -> IO VBO
createBuffer verts =
  let firstIndex = 0
      posVSize = 3
      vPosition = AttribLocation 0
      stride = 0
      numVertices = fromIntegral $ length verts
      vertexSize = fromIntegral $ sizeOf (head verts)
      size = fromIntegral (numVertices * vertexSize)
      quadConfig = do
        withArray verts $ \ptr ->
          bufferData ArrayBuffer $= (size, ptr, StaticDraw)
        setAttribPointer vPosition posVSize stride firstIndex
  in createVBO [quadConfig] Lines firstIndex numVertices

createBufferWithTexture :: [Vertex3 GLfloat] -> [Vertex2 GLfloat] -> IO VBO
createBufferWithTexture verts textCoords =
  let firstVIndex = 0
      posVSize = 3
      texVSize = 2
      firstTIndex = 0
      vPosition = AttribLocation 0
      texcoord = AttribLocation 1
      numVVertices = fromIntegral $ length verts
      numTVertices = fromIntegral $ length textCoords
      vVertexSize = fromIntegral $ sizeOf (head verts)
      tVertexSize = fromIntegral $ sizeOf (head textCoords)
      vsize = fromIntegral (numVVertices * vVertexSize)
      tsize = fromIntegral (numTVertices * tVertexSize)
      stride = 0
      vArrayConfig = do
        withArray verts $ \ptr ->
          bufferData ArrayBuffer $= (vsize, ptr, StaticDraw)
        setAttribPointer vPosition posVSize stride firstVIndex
      tArrayConfig = do
        withArray textCoords $ \ptr ->
          bufferData ArrayBuffer $= (tsize, ptr, StaticDraw)
        setAttribPointer texcoord texVSize stride firstTIndex
  in createVBO [vArrayConfig, tArrayConfig] Triangles firstVIndex numVVertices

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
      ctris = cylinderTriangles 8
      cyb =
        createBufferWithTexture
          (triVertexArray (cylinderVertices 1 0.5 8) ctris)
          (take (length ctris * 3) cylinderTextCoords)
      cywb =
        createBuffer $
        lineVertexArray (cylinderVertices 1 0.5 8) $ cylinderWireframe 8
      stris = sphereTriangles 12
      sb =
        createBufferWithTexture
          (triVertexArray (sphereVertices 0.5 12) stris)
          (take (length stris * 3) sphereTextCoords)
      swb =
        createBuffer $
        lineVertexArray (sphereVertices 0.5 12) $ sphereWireframe 12
  in GeometryBuffers <$> lwb <*> rb <*> rwb <*> cb <*> cwb <*> cyb <*> cywb <*>
     sb <*>
     swb
