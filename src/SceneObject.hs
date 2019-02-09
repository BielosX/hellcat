module SceneObject where

import Linear.Matrix
import BufferedObject
import Shader

data SceneObject = SceneObject {
    object :: BufferedObject,
    localMatrix :: M44 Float,
    program :: Program
}

newSceneObject o p = SceneObject o identity p

drawSceneObject :: SceneObject -> IO ()
drawSceneObject obj = do
    useProgram $ program obj
    drawObject $ object obj
