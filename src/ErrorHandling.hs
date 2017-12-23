module ErrorHandling where

import           Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import           System.IO                 (hPutStrLn, stderr)

printErrors :: IO ()
printErrors = GL.get errors >>= mapM_ (hPutStrLn stderr . ("GL: " ++) . show)

-- type ErrorCallback = Error -> String -> IO ()
glfwErrorCallback :: GLFW.ErrorCallback
glfwErrorCallback _ = hPutStrLn stderr
