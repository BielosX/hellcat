module Camera where

import Linear.Matrix
import Linear.Projection
import Linear.V3

data Camera = Camera {
    camId :: Int,
    projection :: M44 Float,
    view :: M44 Float
}

identityCam = Camera 0 identity identity

perspectiveCam id fov ratio near far = Camera id (perspective fov ratio near far) identity

camLookAt (Camera id p w) eye center up = Camera id p $ lookAt eye center up

transformCam :: Camera -> M44 Float -> Camera
transformCam c m = c { view = newView }
    where newView = m !*! (view c)
