module SceneObject where

import Graphics.GL.Functions
import Graphics.GL.Groups
import Graphics.GL.Types
import Graphics.GL.Tokens

import Linear.Matrix
import BufferedObject
import Shader
import Camera

data SceneObject = SceneObject {
    object :: BufferedObject,
    localMatrix :: M44 Float,
    program :: Program
}

newSceneObject o p = SceneObject o identity p

loadMatrix :: Maybe GLint -> M44 Float -> IO ()
loadMatrix Nothing _ = putStrLn "WARNING: uniform value is not defined"
loadMatrix (Just location) m = uniformMatrix location m

drawSceneObject :: SceneObject -> Camera -> IO ()
drawSceneObject obj activeCamera = do
    useProgram $ program obj
    projLoc <- getUniformLocation (program obj) "projection"
    wordLoc <- getUniformLocation (program obj) "word"
    modelLoc <- getUniformLocation (program obj) "model"
    loadMatrix projLoc (projection activeCamera)
    loadMatrix wordLoc (word activeCamera)
    loadMatrix modelLoc (localMatrix obj)
    drawObject $ object obj
