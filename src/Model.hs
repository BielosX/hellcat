module Model where

data Vertex3 = Vertex3 Float Float Float deriving (Eq, Show)

data TriangleIndex = TriangleIndex Int Int Int deriving (Eq, Show)

data Model = Model {
    vertices :: [Vertex3],
    indices :: [TriangleIndex]
} deriving (Eq, Show)

emptyModel = Model [] []
