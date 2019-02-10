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

drawSceneObject :: SceneObject -> Camera -> IO ()
drawSceneObject obj activeCamera = do
    useProgram $ program obj
    drawObject $ object obj
