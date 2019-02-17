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
import Config
import SceneDescription

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

ratio :: Resolution -> Float
ratio (Resolution width height) = w / h
    where w = fromIntegral width :: Float
          h = fromIntegral height :: Float

someFunc :: ExceptT String IO ()
someFunc = do
    config <- readConfigFile "config.yaml"
    let res = resolution config
    let w = width res
    let h = height res
    initGLFW
    window <- creteWindow w h "Test"
    liftIO $ GLFW.makeContextCurrent (Just window)
    liftIO $ GLFW.setStickyKeysInputMode window GLFW.StickyKeysInputMode'Enabled
    s <- loadScene "scene1.yaml" (toCamera res)
    liftIO $ render window s
