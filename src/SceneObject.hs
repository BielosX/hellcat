module SceneObject where

import Linear.Matrix
import BufferedObject
import Shader

data SceneObject = SceneObject {
    object :: BufferedObject,
    localMatrix :: M44 Float,
    program :: Program
}
