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
import Linear.V3
import Linear.Matrix

import BufferedObject
import Model
import Shader
import SceneObject
import Camera
import Scene
import ObjFile

moveMat = mkTransformationMat identity

render :: GLFW.Window -> Scene -> IO ()
render window s = do
    close <- GLFW.windowShouldClose window
    if close then do
        GLFW.destroyWindow window
        GLFW.terminate
        return ()
    else do
        glClear GL_COLOR_BUFFER_BIT
        GLFW.pollEvents
        k0 <- GLFW.getKey window GLFW.Key'W
        k1 <- GLFW.getKey window GLFW.Key'S
        let newS = case k0 of
                GLFW.KeyState'Pressed -> transformCurrentCam s (moveMat $ V3 0 0 0.1)
                _ -> case k1 of
                        GLFW.KeyState'Pressed -> transformCurrentCam s (moveMat $ V3 0 0 (-0.1))
                        _ -> s
        drawScene newS
        GLFW.swapBuffers window
        render window newS

triangle1v = [Model.Vertex3 0 0 0, Model.Vertex3 0 1 0, Model.Vertex3 1 0 0]
triangle2v = [Model.Vertex3 0 0 0, Model.Vertex3 1 1 0, Model.Vertex3 1 0 0]

idxs = [TriangleIndex 0 1 2]

shader = do
    v <- loadShader VertexShader "shader.vert"
    f <- loadShader FragmentShader "shader.frag"
    p <- createProgram [v, f]
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
    objFile <- liftIO $ readObjFile "monkey.obj"
    m1 <- liftIO $ loadModel $ Model triangle1v idxs
    m2 <- liftIO $ loadModel $ Model triangle2v idxs
    m3 <- liftIO $ loadModel objFile
    let so1 = newSceneObject m1 prog
    let so2 = newSceneObject m2 prog
    let so3 = newSceneObject m3 prog
    let proj = perspectiveCam (pi/4.0) (800.0/600.0) 0.1 100.0
    s <- liftEither $ scene [so3] [proj]
    liftIO $ render window s
