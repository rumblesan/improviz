{-# LANGUAGE OverloadedStrings #-}

module Gfx.Geometries
  ( Geometries
  , GeometryBuffers(..)
  , createAllGeometries
  ) where

import           Data.Int                       ( Int32 )
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import qualified Data.Vector                   as V
import           Data.Vector                    ( (!?)
                                                , Vector
                                                )
import           Data.Word                      ( Word32 )

import           Codec.Wavefront                ( Element(..)
                                                , Face(..)
                                                , FaceIndex(..)
                                                , Location(..)
                                                , Normal(..)
                                                , TexCoord(..)
                                                , WavefrontOBJ(..)
                                                )
import           Graphics.Rendering.OpenGL      ( AttribLocation(..) )
import           Linear.V2                      ( V2(..) )
import           Linear.V3                      ( V3(..) )
import qualified Linear.V3                     as L3
import           Linear.Vector                  ( (^-^) )

import           Configuration.Geometries       ( OBJGeometryConfig(..)
                                                , loadGeometryFolders
                                                )
import           Gfx.VAO                        ( VAO )
import qualified Gfx.VAO                       as VAO
import qualified Gfx.VertexDataBuffer          as VDB
import qualified Gfx.VertexIndexBuffer         as VIB
import           Logging                        ( logInfo )

  {-
data GeometryData = GeometryData { vertices :: V3 Float
                                 , textureCoords :: V2 Float
                                 , normals :: V3 Float
                                 , barycentrics :: V3 Float
                                 , indices :: Word32
                                 }
                                 -}

data GeometryFace = GeometryFace
  { faceVerts      :: [V3 Float]
  , faceTextCoords :: [V2 Float]
  , faceNormals    :: [V3 Float]
  , faceBaryCoords :: [V3 Float]
  }
  deriving (Show, Eq)

data GeometryBuffers = GeometryBuffers
  { vao       :: VAO
  , vertCount :: Int32
  }
  deriving (Show, Eq)

type Geometries = M.Map String GeometryBuffers

loc2Vert3 :: Location -> V3 Float
loc2Vert3 (Location x y z _) = V3 x y z

tex2Vert2 :: TexCoord -> V2 Float
tex2Vert2 (TexCoord x y _) = V2 x y

norm2Vert3 :: Normal -> V3 Float
norm2Vert3 (Normal x y z) = V3 x y z

objFaceToGeoFace
  :: Vector (V3 Float)
  -> Vector (V2 Float)
  -> Vector (V3 Float)
  -> Bool
  -> Int
  -> Face
  -> Maybe GeometryFace
objFaceToGeoFace verts texCoords normals rmCrossbar idx face = do
  fv <- objFaceVerts verts face
  let ftc = fromMaybe (calcTexCoords idx) (objFaceTexCoords texCoords face)
  let ftn = fromMaybe (calculateNormals fv) (objFaceNormals normals face)
  let fbc = calcBaryCoords idx rmCrossbar
  return $ GeometryFace fv ftc ftn fbc


objFaceVerts :: Vector (V3 Float) -> Face -> Maybe [V3 Float]
objFaceVerts vertList (Face xIdx yIdx zIdx _) = do
  x <- vertList !? (faceLocIndex xIdx - 1)
  y <- vertList !? (faceLocIndex yIdx - 1)
  z <- vertList !? (faceLocIndex zIdx - 1)
  return [x, y, z]

objFaceTexCoords :: Vector (V2 Float) -> Face -> Maybe [V2 Float]
objFaceTexCoords texCList (Face xIdx yIdx zIdx _) = do
  xCoordIdx <- faceTexCoordIndex xIdx
  x         <- texCList !? (xCoordIdx - 1)
  yCoordIdx <- faceTexCoordIndex yIdx
  y         <- texCList !? (yCoordIdx - 1)
  zCoordIdx <- faceTexCoordIndex zIdx
  z         <- texCList !? (zCoordIdx - 1)
  return [x, y, z]

objFaceNormals :: Vector (V3 Float) -> Face -> Maybe [V3 Float]
objFaceNormals normList (Face xIdx yIdx zIdx _) = do
  xCoordIdx <- faceTexCoordIndex xIdx
  x         <- normList !? (xCoordIdx - 1)
  yCoordIdx <- faceTexCoordIndex yIdx
  y         <- normList !? (yCoordIdx - 1)
  zCoordIdx <- faceTexCoordIndex zIdx
  z         <- normList !? (zCoordIdx - 1)
  return [x, y, z]

objGeometryFaces :: OBJGeometryConfig -> [GeometryFace]
objGeometryFaces cfg =
  let obj       = objData cfg
      verts     = loc2Vert3 <$> objLocations obj
      texCoords = tex2Vert2 <$> objTexCoords obj
      norms     = norm2Vert3 <$> objNormals obj
      faces     = V.toList $ elValue <$> objFaces obj
  in  catMaybes
        $ uncurry (objFaceToGeoFace verts texCoords norms (removeCrossbar cfg))
        <$> zip [0 ..] faces

calcIndices :: Int -> [Word32]
calcIndices count = take count [0 ..]

calcTexCoords :: Int -> [V2 Float]
calcTexCoords idx = if idx `mod` 2 == 0
  then [V2 1 1, V2 0 1, V2 0 0]
  else [V2 0 0, V2 1 0, V2 1 1]

calcBaryCoords :: Int -> Bool -> [V3 Float]
calcBaryCoords idx rmCrossbar =
  let v = if rmCrossbar then 1 else 0
  in  if idx `mod` 2 == 0
        then [V3 0 v 1, V3 0 1 0, V3 1 0 0]
        else [V3 0 v 1, V3 1 0 0, V3 0 1 0]

calculateNormals :: [V3 Float] -> [V3 Float]
calculateNormals [v1, v2, v3] =
  let n = L3.cross (v1 ^-^ v2) (v2 ^-^ v3) in [n, n, n]
calculateNormals _ = []

objToGeoData :: OBJGeometryConfig -> IO (String, GeometryBuffers)
objToGeoData cfg =
  let
    faces      = objGeometryFaces cfg
    verts      = L.concat $ faceVerts <$> faces
    indices    = calcIndices (length verts)
    texCoords  = L.concat $ faceTextCoords <$> faces
    baryCoords = L.concat $ faceBaryCoords <$> faces
    normals    = L.concat $ faceNormals <$> faces
  in
    do
      indicesBuffer <- VIB.create indices
      vertBuffer    <- VDB.create verts 3
      texCBuffer    <- VDB.create texCoords 2
      baryCBuffer   <- VDB.create baryCoords 3
      normalBuffer  <- VDB.create normals 3
      vao           <- VAO.create
        indicesBuffer
        [ (AttribLocation 0, vertBuffer)
        , (AttribLocation 1, texCBuffer)
        , (AttribLocation 2, baryCBuffer)
        , (AttribLocation 3, normalBuffer)
        ]
      return
        (geometryName cfg, GeometryBuffers vao (VDB.vertexCount vertBuffer))

createAllGeometries :: [FilePath] -> IO Geometries
createAllGeometries folders = do
  objGeoConfig <- loadGeometryFolders folders
  geometries   <- mapM objToGeoData objGeoConfig
  logInfo $ "Loaded " ++ show (length geometries) ++ " geometry files"
  return $ M.fromList geometries
