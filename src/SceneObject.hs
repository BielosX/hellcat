module SceneObject where

import Graphics.GL.Functions
import Graphics.GL.Groups
import Graphics.GL.Types
import Graphics.GL.Tokens

import Linear.Matrix

import BufferedObject
import Shader

data SceneObject = SceneObject {
    object :: BufferedObject,
    modelMatrix :: M44 Float,
    program :: Program
}

newSceneObject o p = SceneObject o identity p

