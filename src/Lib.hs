module Lib
    ( someFunc
    ) where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Functions
import Graphics.GL.Groups
import Graphics.GL.Types
import Graphics.GL.Tokens
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr
import Control.Monad
import Control.Monad.Except
import Data.Either

import BufferedObject
import Model
import Shader
import SceneObject

render :: GLFW.Window -> [SceneObject] -> Int -> IO ()
render window bo i = do
    close <- GLFW.windowShouldClose window
    if close then do
        GLFW.destroyWindow window
        GLFW.terminate
        return ()
    else do
        glClear GL_COLOR_BUFFER_BIT
        GLFW.pollEvents
        k0 <- GLFW.getKey window GLFW.Key'A
        k1 <- GLFW.getKey window GLFW.Key'S
        drawSceneObject (bo !! i)
        GLFW.swapBuffers window
        case k0 of
            GLFW.KeyState'Pressed -> render window bo 0
            _ -> case k1 of
                    GLFW.KeyState'Pressed -> render window bo 1
                    _ -> render window bo i

triangle1v = [Model.Vertex3 0 0 0, Model.Vertex3 0 1 0, Model.Vertex3 1 0 0]
triangle2v = [Model.Vertex3 0 0 0, Model.Vertex3 1 1 0, Model.Vertex3 1 0 0]

idxs = [TriangleIndex 0 1 2]

shader = do
    s <- loadShader FragmentShader "frag.glsl"
    p <- createProgram [s]
    return p

initGLFW :: ExceptT String IO ()
initGLFW = do
    r <- liftIO $ GLFW.init
    if not r then throwError "Unable to initialize GLFW"
    else return ()

creteWindow :: Int -> Int -> String -> ExceptT String IO GLFW.Window
creteWindow w h n = do
    w <- liftIO $ GLFW.createWindow w h n Nothing Nothing
    case w of
        (Just window) -> return window
        Nothing -> do
             liftIO $ GLFW.terminate
             throwError "Unable to create window"

someFunc :: ExceptT String IO ()
someFunc = do
    initGLFW
    window <- creteWindow 800 600 "Test"
    liftIO $ GLFW.makeContextCurrent (Just window)
    liftIO $ GLFW.setStickyKeysInputMode window GLFW.StickyKeysInputMode'Enabled
    prog <- shader
    m1 <- liftIO $ loadModel $ Model triangle1v idxs
    m2 <- liftIO $ loadModel $ Model triangle2v idxs
    let so1 = newSceneObject m1 prog
    let so2 = newSceneObject m2 prog
    liftIO $ render window [so1, so2] 0
