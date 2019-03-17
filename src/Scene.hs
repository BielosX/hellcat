{-# LANGUAGE NamedFieldPuns #-}
module Scene (
    scene,
    Scene,
    drawScene,
    transformCurrentCam,
    Lights(..),
    ObjectRef(..),
    transformSceneElem
    ) where

import Graphics.GL.Types
import Graphics.GL.Functions
import Data.List
import Linear.Matrix
import Linear.V3
import Data.Maybe

import Camera
import SceneObject
import BufferedObject
import Shader
import Light
import Texture

data Lights = Lights {
    lights :: [Light],
    pointLightsCoordsBuf :: ShaderStorageBuffer,
    pointLightsIntens :: ShaderStorageBuffer
}

data Scene = Scene {
    objects :: [SceneObject],
    cameras :: [Camera],
    sceneLights :: Lights,
    currentCam :: Int
}

data ObjectRef = SceneObjectRef Int |
                 CameraRef Int deriving (Eq, Show)

transformSceneElem :: Scene -> ObjectRef -> M44 Float -> Scene
transformSceneElem s (SceneObjectRef r) mat = s { objects = newObj }
    where newObj = updateObjWithId r (objects s) objectId f
          f = \obj -> obj { modelMatrix = mat !*! (modelMatrix obj) }
transformSceneElem s (CameraRef r) mat = s { cameras = newCam }
    where newCam = updateObjWithId r (cameras s) camId f
          f = \c -> c { view = mat !*! (view c) }

updateObjWithId :: Int -> [a] -> (a -> Int) -> (a -> a) -> [a]
updateObjWithId _ [] _ _  = []
updateObjWithId id (x:xs) toId f | toId x == id = (f x):(updateObjWithId id xs toId f)
                                 | otherwise = x:(updateObjWithId id xs toId f)

at :: Int -> [a] -> Maybe a
at 0 [] = Nothing
at 0 (x:xs) = Just x
at n [] = Nothing
at n (x:xs) = at (n-1) xs

updateAt :: Int -> [a] -> (a -> a) -> [a]
updateAt 0 (x:xs) f = (f x):xs
updateAt _ [] _ = []
updateAt n (x:xs) f = x:(updateAt (n-1) xs f)

scene :: [SceneObject] -> [Camera] -> Lights -> Either String Scene
scene [] _ _ = Left "empty scene not allowed"
scene _ [] _ = Left "scene without camera not allowed"
scene o c l = Right $ Scene o c l 0

loadMatrix :: Maybe GLint -> M44 Float -> IO ()
loadMatrix Nothing _ = putStrLn "WARNING: uniform value is not defined"
loadMatrix (Just location) m = uniformMatrix location m

bindTexture :: Maybe BufferedTexture -> IO ()
bindTexture (Just t) = useTexture t
bindTexture Nothing = return ()

uniformVector3f :: V3 Float -> GLint -> IO ()
uniformVector3f (V3 x y z) id = glUniform3f id (f x) (f y) (f z)
    where f = (realToFrac :: Float -> GLfloat)

positionNotDefined = "WARNING: uniform position not defined"

drawSceneObject :: SceneObject -> Camera -> IO ()
drawSceneObject SceneObject{program, object, texture, modelMatrix} activeCamera = do
    useProgram program
    projLoc <- getUniformLocation program "projection"
    viewLoc <- getUniformLocation program "view"
    modelLoc <- getUniformLocation program "model"
    loadMatrix projLoc (projection activeCamera)
    loadMatrix viewLoc (view activeCamera)
    loadMatrix modelLoc modelMatrix
    bindTexture texture
    drawObject object
drawSceneObject Sprite{spriteObject, spritePosition, spriteProgram, spriteTexture} activeCamera = do
    useProgram spriteProgram
    viewLoc <- getUniformLocation spriteProgram "view"
    posLoc <- getUniformLocation spriteProgram "position"
    loadMatrix viewLoc (view activeCamera)
    maybe (putStrLn positionNotDefined) (uniformVector3f spritePosition) posLoc
    useTexture spriteTexture
    drawObject spriteObject

drawScene :: Scene -> IO ()
drawScene s = do
    let cam = at (currentCam s) (cameras s)
    case cam of
        Nothing -> return ()
        (Just c) -> do
            bindSSBBase (pointLightsCoordsBuf $ sceneLights s) 3
            bindSSBBase (pointLightsIntens $ sceneLights s) 4
            mapM_ (\o -> drawSceneObject o c) (objects s)

transformCurrentCam :: Scene -> M44 Float -> Scene
transformCurrentCam s m = s { cameras = newCameras }
    where   current = currentCam s
            newCameras = updateAt current (cameras s) (\c -> transformCam c m)

