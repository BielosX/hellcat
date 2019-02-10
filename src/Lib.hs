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

someFunc :: IO ()
someFunc = do
    r <- GLFW.init
    if not r then do
        putStrLn "Unable to initialize GLFW"
        return ()
    else do
        w <- GLFW.createWindow 800 600 "Test" Nothing Nothing
        case w of
            (Just window) -> do
                GLFW.makeContextCurrent (Just window)
                GLFW.setStickyKeysInputMode window GLFW.StickyKeysInputMode'Enabled
                prog <- runExceptT $ shader
                case prog of
                    (Left e) -> putStrLn e
                    (Right p) -> return ()
                let program = fromRight (Program 0) prog
                m1 <- loadModel $ Model triangle1v idxs
                m2 <- loadModel $ Model triangle2v idxs
                let so1 = newSceneObject m1 program
                let so2 = newSceneObject m2 program
                render window [so1, so2] 0
            Nothing -> do
                putStrLn "Unable to create window"
                GLFW.terminate
