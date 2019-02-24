module Light where

import Linear.V4

data Light = PointLight {
    intensity :: Float,
    position :: V4 Float
} deriving (Show, Eq)

isPointLight (PointLight _ _) = True
