module SceneObject where

import Graphics.GL.Functions
import Graphics.GL.Groups
import Graphics.GL.Types
import Graphics.GL.Tokens
import Codec.Picture.Types
import Control.Monad.Except
import Data.List

import Linear.Matrix
import Linear.V3

import BufferedObject
import Shader
import Texture
import Model

data SceneObject = SceneObject {
    objectId :: Int,
    object :: BufferedObject,
    modelMatrix :: M44 Float,
    program :: Program,
    texture :: Maybe BufferedTexture
} | Sprite {
    spriteObjectId :: Int,
    spriteObject :: BufferedObject,
    spritePosition :: V3 Float,
    spriteProgram :: Program,
    spriteTexture :: BufferedTexture
}

newSceneObject id o p = SceneObject id o identity p Nothing

spriteFromImage :: DynamicImage -> Program -> V3 Float -> Int -> ExceptT String IO SceneObject
spriteFromImage di@(ImageRGBA8 i) prog pos id = do
    let width = imageWidth i
    let height = imageHeight i
    let model = Model (spriteVertices width height) spriteNormals [] spriteUVs
    vao <- liftIO $ loadModel model
    texture <- loadImage di
    return $ Sprite id vao pos prog texture
spriteFromImage _ _ _ _ = throwError "texture should contain alpha channel"

spriteFromFile :: FilePath -> Program -> V3 Float -> Int -> ExceptT String IO SceneObject
spriteFromFile path prog pos id = do
    image <- loadDynamicImage path
    spriteFromImage image prog pos id

spriteVertices :: Int -> Int -> [Model.Vector3]
spriteVertices width height = fmap f [
        Model.Vector3 (-0.5) (-0.5) 0,
        Model.Vector3 0.5 (-0.5) 0,
        Model.Vector3 (-0.5) 0.5 0,
        Model.Vector3 0.5 (-0.5) 0,
        Model.Vector3 0.5 0.5 0,
        Model.Vector3 (-0.5) 0.5 0
    ]
    where maxLen = toFloat $ max width height
          normW = (toFloat width) / maxLen
          normH = (toFloat height) / maxLen
          toFloat = (fromIntegral :: Int -> Float)
          f (Model.Vector3 x y z) = Model.Vector3 (x * normW) (y * normH) z

spriteNormals :: [Model.Vector3]
spriteNormals = take 6 $ repeat $ Model.Vector3 0 0 1

spriteUVs :: [Model.Vector2]
spriteUVs = [
        Model.Vector2 0.0 0.0,
        Model.Vector2 1.0 0.0,
        Model.Vector2 0.0 1.0,
        Model.Vector2 1.0 0.0,
        Model.Vector2 1.0 1.0,
        Model.Vector2 0.0 1.0
    ]
