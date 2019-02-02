module Gfx.VertexBuffers
  ( VBO(..)
  , setAttribPointer
  , createVBO
  , drawVBO
  , deleteVBO
  )
where

import           Graphics.Rendering.OpenGL     as GL

import           Control.Monad                  ( forM
                                                , forM_
                                                )
import           Foreign.Ptr                    ( Ptr
                                                , nullPtr
                                                , plusPtr
                                                )
import           GHC.Int                        ( Int32 )

data VBO =
  VBO VertexArrayObject
      [BufferObject]
      PrimitiveMode
      ArrayIndex
      NumArrayIndices
  deriving (Show, Eq)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

setAttribPointer :: AttribLocation -> Int32 -> Int32 -> Int32 -> IO ()
setAttribPointer location numComponents stride idx = do
  vertexAttribPointer location
    $= ( ToFloat
       , VertexArrayDescriptor numComponents Float stride (bufferOffset idx)
       )
  vertexAttribArray location $= Enabled

createVBO :: [IO ()] -> PrimitiveMode -> ArrayIndex -> NumArrayIndices -> IO VBO
createVBO arrayConfigFuncs primMode arrIdx numVertices = do
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  arrayBuffers <- forM
    arrayConfigFuncs
    (\arrayConfig -> do
      arrayBuffer <- GL.genObjectName
      GL.bindBuffer ArrayBuffer $= Just arrayBuffer
      arrayConfig
      GL.bindBuffer ArrayBuffer $= Nothing
      return arrayBuffer
    )
  GL.bindVertexArrayObject $= Nothing
  return $ VBO vao arrayBuffers primMode arrIdx numVertices

drawVBO :: VBO -> IO ()
drawVBO (VBO bufferObject arrayBuffers primMode arrayIndex numVertices) = do
  GL.bindVertexArrayObject $= Just bufferObject
  GL.drawArrays primMode arrayIndex numVertices

deleteVBO :: VBO -> IO ()
deleteVBO (VBO bufferObject arrayBuffers _ _ _) = do
  forM_ arrayBuffers GL.deleteObjectName
  GL.deleteObjectName bufferObject
