module Camera where

import Linear.Matrix

data Camera = Camera {
    perspective :: M44 Float,
    word :: M44 Float
}

identityCam = Camera identity identity
