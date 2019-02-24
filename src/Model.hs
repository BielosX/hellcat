module Model where

data Vector3 = Vector3 Float Float Float deriving (Eq, Show)

data TriangleIndex = TriangleIndex Int Int Int deriving (Eq, Show)

data Model = Model {
    vertices :: [Vector3],
    indices :: [TriangleIndex]
} deriving (Eq, Show)

emptyModel = Model [] []
