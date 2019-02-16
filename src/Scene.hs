module Scene(scene, Scene, drawScene, transformCurrentCam) where

import Data.List
import Linear.Matrix

import Camera
import SceneObject

data Scene = Scene {
    objects :: [SceneObject],
    cameras :: [Camera],
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

scene :: [SceneObject] -> [Camera] -> Either String Scene
scene [] _ = Left "empty scene not allowed"
scene _ [] = Left "scene without camera not allowed"
scene o c = Right $ Scene o c 0

drawScene :: Scene -> IO ()
drawScene s = do
    let cam = at (currentCam s) (cameras s)
    case cam of
        Nothing -> return ()
        (Just c) -> mapM_ (\o -> drawSceneObject o c) (objects s)

transformCurrentCam :: Scene -> M44 Float -> Scene
transformCurrentCam s m = s { cameras = newCameras }
    where   current = currentCam s
            newCameras = updateAt current (cameras s) (\c -> transformCam c m)

