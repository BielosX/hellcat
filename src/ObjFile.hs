module ObjFile where

import ObjLexer
import ObjParser

import Data.Text.Lazy.IO as I
import Data.Text.Lazy as T
import System.IO
import Data.List as L

data Vertex3 = Vertex3 Float Float Float deriving (Eq, Show)

data TriangleIndex = TriangleIndex Int Int Int deriving (Eq, Show)

data Model = Model {
    vertices :: [Vertex3],
    indices :: [TriangleIndex]
} deriving (Eq, Show)

emptyModel = Model [] []

readObjFile :: FilePath -> IO Model
readObjFile path = do
    content <- I.readFile path
    return $ parseLines (fmap T.unpack $ T.lines content) emptyModel

parseLines :: [String] -> Model -> Model
parseLines [] m = Model (L.reverse $ vertices m) (L.reverse $ indices m)
parseLines (x:xs) m = let tokens = lexer x in
                        case tokens of
                            [] -> parseLines xs m
                            t -> parseLines xs $ updateModel (objParse t) m

updateModel :: Value -> Model -> Model
updateModel (ObjParser.Vertex3 x y z) (Model v i) = Model (v3:v) i
    where v3 = ObjFile.Vertex3 x y z
updateModel (ObjParser.FaceDef a1 a2 a3) (Model v i) = Model v (idx:i)
    where idx = TriangleIndex (vIdx a1 - 1) (vIdx a2 - 1) (vIdx a3 - 1)
updateModel _ m = m
