module Model where

data Vector2 = Vector2 Float Float deriving (Eq, Show)

data Vector3 = Vector3 Float Float Float deriving (Eq, Show)

data TriangleIndex = TriangleIndex Int Int Int deriving (Eq, Show)

data Model = Model {
    vertices :: [Vector3],
    normals :: [Vector3],
    indices :: [TriangleIndex],
    uvs :: [Vector2]
} deriving (Eq, Show)

emptyModel = Model [] [] [] []
