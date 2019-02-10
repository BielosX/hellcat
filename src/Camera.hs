module Camera where

import Linear.Matrix
import Linear.Projection

data Camera = Camera {
    projection :: M44 Float,
    word :: M44 Float
}

identityCam = Camera identity identity

perspectiveCam fov ratio near far = Camera (perspective fov ratio near far) identity

camLookAt (Camera p w) eye center up = Camera p $ lookAt eye center up
