module ErrorHandling where

import           Graphics.Rendering.OpenGL     as GL
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

printErrors :: IO ()
printErrors = GL.get errors >>= mapM_ (hPutStrLn stderr . ("GL: " ++) . show)
