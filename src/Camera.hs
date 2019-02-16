module Camera where

import Linear.Matrix
import Linear.Projection
import Linear.V3

data Camera = Camera {
    projection :: M44 Float,
    word :: M44 Float
}

identityCam = Camera identity identity

perspectiveCam fov ratio near far = Camera (perspective fov ratio near far) identity

camLookAt (Camera p w) eye center up = Camera p $ lookAt eye center up

transformCam :: Camera -> M44 Float -> Camera
transformCam c m = c { word = newWord }
    where newWord = m !*! (word c)
