module SceneObject where

import Linear.Matrix
import VertexArrayObject

data SceneObject = SceneObject {
    vao :: VertexArrayObject,
    localMatrix :: M44 Float
}
