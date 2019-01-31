module ObjFile where

import ObjLexer
import ObjParser
import Model as M

import Data.Text.Lazy.IO as I
import Data.Text.Lazy as T
import System.IO
import Data.List as L


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
    where v3 = M.Vertex3 x y z
updateModel (ObjParser.FaceDef a1 a2 a3) (Model v i) = Model v (idx:i)
    where idx = TriangleIndex (vIdx a1 - 1) (vIdx a2 - 1) (vIdx a3 - 1)
updateModel _ m = m
