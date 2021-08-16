module Gfx.OpenGL where

import           Gfx.Types                      ( Colour(..) )
import qualified Graphics.Rendering.OpenGL     as GL
import           Graphics.Rendering.OpenGL      ( Color4(..)
                                                , GLfloat
                                                , Vector2(..)
                                                , Vector3(..)
                                                , Vector4(..)
                                                , errors
                                                , get
                                                )
import           Graphics.Rendering.OpenGL      ( ($=) )
import           Graphics.Rendering.OpenGL.GL.Shaders.Attribs
                                               as GLS
import           Language.Ast                   ( Value(..) )

import           Logging                        ( logError )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

valueGetFloat :: Value -> Either String GLfloat
valueGetFloat (Number v) = Right v
valueGetFloat _          = Left "Expected number"

valueToUniform :: Value -> GL.VariableType -> GL.UniformLocation -> IO ()
valueToUniform value utype uniformLoc = case utype of
  GLS.Float'    -> valueToGLfloatUniform value uniformLoc
  GLS.FloatVec2 -> valueToVec2Uniform value uniformLoc
  GLS.FloatVec3 -> valueToVec3Uniform value uniformLoc
  GLS.FloatVec4 -> valueToVec4Uniform value uniformLoc

valueToGLfloatUniform :: Value -> GL.UniformLocation -> IO ()
valueToGLfloatUniform value uniformLoc =
  either logError (GL.uniform uniformLoc $=) (valueToGLfloat value)

valueToVec2Uniform :: Value -> GL.UniformLocation -> IO ()
valueToVec2Uniform value uniformLoc =
  either logError (GL.uniform uniformLoc $=) (valueToGLfloatVec2 value)

valueToVec3Uniform :: Value -> GL.UniformLocation -> IO ()
valueToVec3Uniform value uniformLoc =
  either logError (GL.uniform uniformLoc $=) (valueToGLfloatVec3 value)

valueToVec4Uniform :: Value -> GL.UniformLocation -> IO ()
valueToVec4Uniform value uniformLoc =
  either logError (GL.uniform uniformLoc $=) (valueToGLfloatVec4 value)

valueToGLfloat :: Value -> Either String GLfloat
valueToGLfloat (Number v      ) = Right v
valueToGLfloat (VList  valList) = case valList of
  x : _ -> valueGetFloat x
  []    -> Left "Empty List"
valueToGLfloat _ = Left "Invalid value"

valueToGLfloatVec2 :: Value -> Either String (Vector2 GLfloat)
valueToGLfloatVec2 (Number v      ) = Right $ Vector2 v v
valueToGLfloatVec2 (VList  valList) = case valList of
  x : y : _ -> Vector2 <$> (valueGetFloat x) <*> (valueGetFloat y)
  x     : _ -> Vector2 <$> (valueGetFloat x) <*> (valueGetFloat x)
  []        -> Left "Empty List"
valueToGLfloatVec2 _ = Left "Invalid value"

valueToGLfloatVec3 :: Value -> Either String (Vector3 GLfloat)
valueToGLfloatVec3 (Number v      ) = Right $ Vector3 v v v
valueToGLfloatVec3 (VList  valList) = case valList of
  x : y : z : _ ->
    Vector3 <$> (valueGetFloat x) <*> (valueGetFloat y) <*> (valueGetFloat z)
  x : y : _ ->
    Vector3 <$> (valueGetFloat x) <*> (valueGetFloat y) <*> (valueGetFloat y)
  x : _ ->
    Vector3 <$> (valueGetFloat x) <*> (valueGetFloat x) <*> (valueGetFloat x)
  [] -> Left "Empty List"
valueToGLfloatVec3 _ = Left "Invalid value"

valueToGLfloatVec4 :: Value -> Either String (Vector4 GLfloat)
valueToGLfloatVec4 (Number v      ) = Right $ Vector4 v v v v
valueToGLfloatVec4 (VList  valList) = case valList of
  x : y : z : w : _ ->
    Vector4
      <$> (valueGetFloat x)
      <*> (valueGetFloat y)
      <*> (valueGetFloat z)
      <*> (valueGetFloat w)
  x : y : z : _ ->
    Vector4
      <$> (valueGetFloat x)
      <*> (valueGetFloat y)
      <*> (valueGetFloat z)
      <*> (pure 1)
  x : y : _ ->
    Vector4
      <$> (valueGetFloat x)
      <*> (valueGetFloat y)
      <*> (valueGetFloat y)
      <*> (pure 1)
  x : _ ->
    Vector4
      <$> (valueGetFloat x)
      <*> (valueGetFloat x)
      <*> (valueGetFloat x)
      <*> (pure 1)
  [] -> Left "Empty List"
valueToGLfloatVec4 _ = Left "Invalid value"



colToGLCol :: Colour -> Color4 GLfloat
colToGLCol (Colour r g b a) = Color4 r g b a

printErrors :: IO ()
printErrors = get errors >>= mapM_ (hPutStrLn stderr . ("GL: " ++) . show)

printErrorsDBG :: String -> IO ()
printErrorsDBG msg =
  print msg >> get errors >>= mapM_ (hPutStrLn stderr . ("GL: " ++) . show)
