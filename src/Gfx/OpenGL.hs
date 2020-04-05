module Gfx.OpenGL where

import           Gfx.Types                      ( Colour(..) )
import           Graphics.Rendering.OpenGL      ( Color4(..)
                                                , GLfloat
                                                , get
                                                , errors
                                                )

import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

colToGLCol :: Colour -> Color4 GLfloat
colToGLCol (Colour r g b a) = Color4 r g b a

printErrors :: IO ()
printErrors = get errors >>= mapM_ (hPutStrLn stderr . ("GL: " ++) . show)

printErrorsDBG :: String -> IO ()
printErrorsDBG msg =
  print msg >> get errors >>= mapM_ (hPutStrLn stderr . ("GL: " ++) . show)
