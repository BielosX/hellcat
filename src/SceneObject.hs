module SceneObject where

import Graphics.GL.Functions
import Graphics.GL.Groups
import Graphics.GL.Types
import Graphics.GL.Tokens

import Linear.Matrix

import BufferedObject
import Shader
import Texture

data SceneObject = SceneObject {
    objectId :: Int,
    object :: BufferedObject,
    modelMatrix :: M44 Float,
    program :: Program,
    texture :: Maybe BufferedTexture
}

newSceneObject id o p = SceneObject id o identity p Nothing

