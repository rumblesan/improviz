module Gfx.VertexBuffers where

import           Graphics.Rendering.OpenGL as GL

import           Control.Monad             (forM, forM_)
import           Foreign.Ptr               (Ptr, nullPtr, plusPtr)
import           GHC.Int                   (Int32)

data VBO =
  VBO VertexArrayObject
      [BufferObject]
      ArrayIndex
      NumArrayIndices
  deriving (Show, Eq)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

setAttribPointer :: AttribLocation -> Int32 -> Int32 -> Int32 -> IO ()
setAttribPointer location numComponents stride idx = do
  vertexAttribPointer location $=
    ( ToFloat
    , VertexArrayDescriptor numComponents Float stride (bufferOffset idx))
  vertexAttribArray location $= Enabled

createVBO :: [IO ()] -> ArrayIndex -> NumArrayIndices -> IO VBO
createVBO arrayConfigFuncs arrIdx numVertices = do
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  arrayBuffers <-
    forM
      arrayConfigFuncs
      (\arrayConfig -> do
         arrayBuffer <- GL.genObjectName
         GL.bindBuffer ArrayBuffer $= Just arrayBuffer
         arrayConfig
         GL.bindBuffer ArrayBuffer $= Nothing
         return arrayBuffer)
  GL.bindVertexArrayObject $= Nothing
  return $ VBO vao arrayBuffers arrIdx numVertices

drawVBO :: VBO -> IO ()
drawVBO (VBO bufferObject arrayBuffers arrayIndex numVertices) = do
  GL.bindVertexArrayObject $= Just bufferObject
  GL.drawArrays Triangles arrayIndex numVertices

deleteVBO :: VBO -> IO ()
deleteVBO (VBO bufferObject arrayBuffers _ _) = do
  forM_ arrayBuffers GL.deleteObjectName
  GL.deleteObjectName bufferObject
