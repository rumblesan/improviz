{-# LANGUAGE TupleSections #-}

module Gfx.GeometryBuffers
  ( GeometryBuffers
  , ShapeBuffer(..)
  , createAllBuffers
  )
where

import qualified Data.Vector                   as V
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import           Data.Vector                    ( (!?) )
import           Foreign.Marshal.Array          ( withArray )
import           Foreign.Storable               ( sizeOf )

import qualified Graphics.Rendering.OpenGL     as GL
import           Graphics.Rendering.OpenGL      ( Vertex2(..)
                                                , GLfloat
                                                , Vertex3(..)
                                                , AttribLocation(..)
                                                , BufferTarget(ArrayBuffer)
                                                , ($=)
                                                , BufferUsage(StaticDraw)
                                                , PrimitiveMode
                                                  ( Lines
                                                  , Triangles
                                                  )
                                                )

import           Gfx.VertexBuffers              ( VBO
                                                , createVBO
                                                , setAttribPointer
                                                )
import           Codec.Wavefront                ( WavefrontOBJ(..)
                                                , Element(..)
                                                , Face(..)
                                                , FaceIndex(..)
                                                , Location(..)
                                                , Line(..)
                                                , LineIndex(..)
                                                , TexCoord(..)
                                                , fromFile
                                                )

data ShapeBuffer = ShapeBuffer { triangles :: Maybe VBO
                               , wireframe :: Maybe VBO
                               } deriving (Show, Eq)

type GeometryBuffers = M.Map String ShapeBuffer

createBuffer :: [Vertex3 GLfloat] -> IO VBO
createBuffer verts =
  let firstIndex  = 0
      posVSize    = 3
      vPosition   = AttribLocation 0
      stride      = 0
      numVertices = fromIntegral $ length verts
      vertexSize  = fromIntegral $ sizeOf (head verts)
      size        = fromIntegral (numVertices * vertexSize)
      quadConfig  = do
        withArray verts
          $ \ptr -> GL.bufferData ArrayBuffer $= (size, ptr, StaticDraw)
        setAttribPointer vPosition posVSize stride firstIndex
  in  createVBO [quadConfig] Lines firstIndex numVertices

createBufferWithTexture :: [Vertex3 GLfloat] -> [Vertex2 GLfloat] -> IO VBO
createBufferWithTexture verts textCoords =
  let firstVIndex  = 0
      posVSize     = 3
      texVSize     = 2
      firstTIndex  = 0
      vPosition    = AttribLocation 0
      texcoord     = AttribLocation 1
      numVVertices = fromIntegral $ length verts
      numTVertices = fromIntegral $ length textCoords
      vVertexSize  = fromIntegral $ sizeOf (head verts)
      tVertexSize  = fromIntegral $ sizeOf (head textCoords)
      vsize        = fromIntegral (numVVertices * vVertexSize)
      tsize        = fromIntegral (numTVertices * tVertexSize)
      stride       = 0
      vArrayConfig = do
        withArray verts
          $ \ptr -> GL.bufferData ArrayBuffer $= (vsize, ptr, StaticDraw)
        setAttribPointer vPosition posVSize stride firstVIndex
      tArrayConfig = do
        withArray textCoords
          $ \ptr -> GL.bufferData ArrayBuffer $= (tsize, ptr, StaticDraw)
        setAttribPointer texcoord texVSize stride firstTIndex
  in  createVBO [vArrayConfig, tArrayConfig] Triangles firstVIndex numVVertices

loc2Vert3 :: Location -> Vertex3 GLfloat
loc2Vert3 (Location x y z _) = Vertex3 x y z

tex2Vert2 :: TexCoord -> Vertex2 GLfloat
tex2Vert2 (TexCoord x y _) = Vertex2 x y

facesToLineIndexes :: Face -> [(Int, Int)]
facesToLineIndexes (Face xIdx yIdx zIdx rest) =
  let indexes =
          faceLocIndex xIdx
            : faceLocIndex yIdx
            : faceLocIndex zIdx
            : (faceLocIndex <$> rest)
      shifted = tail indexes ++ [head indexes]
  in  zip indexes shifted

dedupLines :: [(Int, Int)] -> [(Int, Int)]
dedupLines lines = M.keys $ M.fromList $ fmap (, 0) lines

lineIdxToLine :: (Int, Int) -> Line
lineIdxToLine (a, b) = Line (LineIndex a Nothing) (LineIndex b Nothing)

objFaceVerts :: WavefrontOBJ -> Maybe [Vertex3 GLfloat]
objFaceVerts obj =
  let verts   = objLocations obj
      locList = V.toList $ faceToVerts verts <$> objFaces obj
  in  L.concat <$> sequence locList
 where
  faceToVerts vertList element = do
    let (Face xIdx yIdx zIdx _) = elValue element
    x <- vertList !? (faceLocIndex xIdx - 1)
    y <- vertList !? (faceLocIndex yIdx - 1)
    z <- vertList !? (faceLocIndex zIdx - 1)
    return [loc2Vert3 x, loc2Vert3 y, loc2Vert3 z]

objTextureCoords :: WavefrontOBJ -> Maybe [Vertex2 GLfloat]
objTextureCoords obj =
  let textCoords = objTexCoords obj
      locList    = V.toList $ faceToVerts textCoords <$> objFaces obj
  in  L.concat <$> sequence locList
 where
  faceToVerts vertList element = do
    let (Face xIdx yIdx zIdx _) = elValue element
    xCoordIdx <- faceTexCoordIndex xIdx
    x         <- vertList !? (xCoordIdx - 1)
    yCoordIdx <- faceTexCoordIndex yIdx
    y         <- vertList !? (yCoordIdx - 1)
    zCoordIdx <- faceTexCoordIndex zIdx
    z         <- vertList !? (zCoordIdx - 1)
    return [tex2Vert2 x, tex2Vert2 y, tex2Vert2 z]

objWireframe :: WavefrontOBJ -> Maybe [Vertex3 GLfloat]
objWireframe obj =
  let verts   = objLocations obj
      faces   = elValue <$> objFaces obj
      lines   = objectLines obj faces
      locList = V.toList $ lineToVerts verts <$> lines
  in  L.concat <$> sequence locList
 where
  lineToVerts vertList (Line xIdx yIdx) = do
    x <- vertList !? (lineLocIndex xIdx - 1)
    y <- vertList !? (lineLocIndex yIdx - 1)
    return [loc2Vert3 x, loc2Vert3 y]
  objectLines obj f = if V.length (objLines obj) > 0
    then elValue <$> objLines obj
    else V.fromList $ lineIdxToLine <$> dedupLines
      (L.concat $ facesToLineIndexes <$> f)



createTrianglesFromObj :: FilePath -> IO VBO
createTrianglesFromObj path = do
  fileInput <- fromFile path
  obj       <- case fileInput of
    Left  err -> fail err
    Right obj -> return obj
  let vtData = (,) <$> objFaceVerts obj <*> objTextureCoords obj
  case vtData of
    Nothing                 -> fail $ "Could not load data from" ++ path
    Just (verts, texCoords) -> createBufferWithTexture verts texCoords

createWireframeFromObj :: FilePath -> IO VBO
createWireframeFromObj path = do
  fileInput <- fromFile path
  obj       <- case fileInput of
    Left  err -> fail err
    Right obj -> return obj
  case objWireframe obj of
    Nothing    -> fail $ "Could not load data from" ++ path
    Just verts -> createBuffer verts


createAllBuffers :: IO GeometryBuffers
createAllBuffers = do
  lwb  <- createWireframeFromObj "geometries/line.obj"
  rb   <- createTrianglesFromObj "geometries/rectangle.obj"
  rwb  <- createWireframeFromObj "geometries/rectangle.obj"
  cb   <- createTrianglesFromObj "geometries/cube.obj"
  cwb  <- createWireframeFromObj "geometries/cube.obj"
  cyb  <- createTrianglesFromObj "geometries/cylinder.obj"
  cywb <- createWireframeFromObj "geometries/cylinder.obj"
  sb   <- createTrianglesFromObj "geometries/sphere.obj"
  swb  <- createWireframeFromObj "geometries/sphere.obj"
  return $ M.fromList
    [ ("line"     , ShapeBuffer Nothing (Just lwb))
    , ("rectangle", ShapeBuffer (Just rb) (Just rwb))
    , ("cube"     , ShapeBuffer (Just cb) (Just cwb))
    , ("cylinder" , ShapeBuffer (Just cyb) (Just cywb))
    , ("sphere"   , ShapeBuffer (Just sb) (Just swb))
    ]
