{-# LANGUAGE OverloadedStrings #-}

module Gfx.Geometries
  ( Geometries
  , GeometryData(..)
  , createAllGeometries
  )
where

import qualified Data.Vector                   as V
import           Data.Vector                    ( (!?) )
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           System.FilePath.Posix          ( (</>) )

import           Graphics.Rendering.OpenGL      ( Vertex2(..)
                                                , GLfloat
                                                , GLuint
                                                , Vertex3(..)
                                                , AttribLocation(..)
                                                , GLsizei
                                                )
import           Codec.Wavefront                ( WavefrontOBJ(..)
                                                , Element(..)
                                                , Face(..)
                                                , FaceIndex(..)
                                                , Location(..)
                                                , TexCoord(..)
                                                , fromFile
                                                )

import           Gfx.VAO                        ( VAO )
import qualified Gfx.VAO                       as VAO
import qualified Gfx.VertexDataBuffer          as VDB
import qualified Gfx.VertexIndexBuffer         as VIB
import           Configuration                  ( loadFolderConfig )
import           Configuration.Geometries
import           Logging                        ( logError
                                                , logInfo
                                                )

data GeometryData = GeometryData { vao :: VAO
                                 , vertCount :: GLsizei
                                 } deriving (Show, Eq)

type Geometries = M.Map String GeometryData

loc2Vert3 :: Location -> Vertex3 GLfloat
loc2Vert3 (Location x y z _) = Vertex3 x y z

tex2Vert2 :: TexCoord -> Vertex2 GLfloat
tex2Vert2 (TexCoord x y _) = Vertex2 x y

defaultTextureCoords :: Int -> [Vertex2 GLfloat]
defaultTextureCoords num = take num $ L.cycle
  [Vertex2 1 1, Vertex2 0 1, Vertex2 0 0, Vertex2 0 0, Vertex2 1 0, Vertex2 1 1]

defaultBarycentricCoords :: Int -> Bool -> [Vertex3 GLfloat]
defaultBarycentricCoords num removeCrossbar =
  let v = if removeCrossbar then 1 else 0
  in  take num $ L.cycle
        [ Vertex3 0 v 1
        , Vertex3 0 1 0
        , Vertex3 1 0 0
        , Vertex3 0 v 1
        , Vertex3 1 0 0
        , Vertex3 0 1 0
        ]


objVerts :: WavefrontOBJ -> [Vertex3 GLfloat]
objVerts obj =
  let verts   = loc2Vert3 <$> objLocations obj
      locList = catMaybes $ V.toList $ faceToVerts verts <$> objFaces obj
  in  L.concat locList
 where
  faceToVerts vertList element = do
    let (Face xIdx yIdx zIdx _) = elValue element
    x <- vertList !? (faceLocIndex xIdx - 1)
    y <- vertList !? (faceLocIndex yIdx - 1)
    z <- vertList !? (faceLocIndex zIdx - 1)
    return [x, y, z]

objTextCoords :: WavefrontOBJ -> Maybe [Vertex2 GLfloat]
objTextCoords obj =
  let coords = tex2Vert2 <$> objTexCoords obj
      texCoords =
          L.concat $ catMaybes $ V.toList $ faceToTexC coords <$> objFaces obj
  in  if null texCoords then Nothing else Just texCoords
 where
  faceToTexC texClist element = do
    let (Face xIdx yIdx zIdx _) = elValue element
    xCoordIdx <- faceTexCoordIndex xIdx
    x         <- texClist !? (xCoordIdx - 1)
    yCoordIdx <- faceTexCoordIndex yIdx
    y         <- texClist !? (yCoordIdx - 1)
    zCoordIdx <- faceTexCoordIndex zIdx
    z         <- texClist !? (zCoordIdx - 1)
    return [x, y, z]

calcIndices :: Int -> [GLuint]
calcIndices count = take count [0 ..]

objToGeoData
  :: GeometryConfig -> WavefrontOBJ -> IO (Maybe (String, GeometryData))
objToGeoData cfg obj =
  let
    verts   = objVerts obj
    indices = calcIndices (length verts)
    texCoords =
      fromMaybe (defaultTextureCoords (length verts)) (objTextCoords obj)
    baryCoords = defaultBarycentricCoords (L.length verts) (removeCrossbar cfg)
  in
    do
      indicesBuffer <- VIB.create indices
      vertBuffer    <- VDB.create verts 3
      texCBuffer    <- VDB.create texCoords 2
      baryCBuffer   <- VDB.create baryCoords 3
      vao           <- VAO.create
        indicesBuffer
        [ (AttribLocation 0, vertBuffer)
        , (AttribLocation 1, texCBuffer)
        , (AttribLocation 2, baryCBuffer)
        ]
      return $ Just
        (geometryName cfg, GeometryData vao (VDB.vertexCount vertBuffer))

createGeometryData
  :: FilePath -> GeometryConfig -> IO (Maybe (String, GeometryData))
createGeometryData folderPath cfg = do
  fileInput <- fromFile $ folderPath </> geometryFile cfg
  case fileInput of
    Left  err -> logError err >> return Nothing
    Right obj -> objToGeoData cfg obj


loadGeometryFolder :: FilePath -> IO [Maybe (String, GeometryData)]
loadGeometryFolder folderPath = do
  folderConfig <- loadFolderConfig folderPath
  case folderConfig of
    Left  err -> logError err >> return []
    Right cfg -> mapM (createGeometryData folderPath) (geometries cfg)

createAllGeometries :: [FilePath] -> IO Geometries
createAllGeometries folders = do
  geometries <- catMaybes . concat <$> mapM loadGeometryFolder folders
  logInfo $ "Loaded " ++ show (length geometries) ++ " geometry files"
  return $ M.fromList geometries
