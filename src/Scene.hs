module Scene(scene, Scene, drawScene) where

import Data.List

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
