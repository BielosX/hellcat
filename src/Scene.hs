module Scene(scene, Scene, drawScene, transformCurrentCam, Lights(..)) where

import Graphics.GL.Types
import Data.List
import Linear.Matrix

import Camera
import SceneObject
import BufferedObject
import Shader
import Light

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

drawSceneObject :: SceneObject -> Camera -> IO ()
drawSceneObject obj activeCamera = do
    useProgram $ program obj
    projLoc <- getUniformLocation (program obj) "projection"
    viewLoc <- getUniformLocation (program obj) "view"
    modelLoc <- getUniformLocation (program obj) "model"
    loadMatrix projLoc (projection activeCamera)
    loadMatrix viewLoc (view activeCamera)
    loadMatrix modelLoc (modelMatrix obj)
    drawObject $ object obj

drawScene :: Scene -> IO ()
drawScene s = do
    let cam = at (currentCam s) (cameras s)
    case cam of
        Nothing -> return ()
        (Just c) -> do
            bindSSBBase (pointLightsCoordsBuf $ sceneLights s) 1
            bindSSBBase (pointLightsIntens $ sceneLights s) 2
            mapM_ (\o -> drawSceneObject o c) (objects s)

transformCurrentCam :: Scene -> M44 Float -> Scene
transformCurrentCam s m = s { cameras = newCameras }
    where   current = currentCam s
            newCameras = updateAt current (cameras s) (\c -> transformCam c m)

