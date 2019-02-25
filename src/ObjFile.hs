module ObjFile where

import ObjLexer
import ObjParser
import Model as M

import Data.Text.Lazy.IO as I
import Data.Text.Lazy as T
import System.IO
import Data.List as L

data ObjData = ObjData {
    v :: [M.Vector3],
    n :: [M.Vector3],
    vecIndices :: [TriangleIndex],
    normIndices :: [TriangleIndex]
}

emptyData = ObjData [] [] [] []

readObjFile :: FilePath -> IO Model
readObjFile path = do
    content <- I.readFile path
    let d = parseLines (fmap T.unpack $ T.lines content) emptyData
    return $ M.Model (v d) (vecIndices d)

parseLines :: [String] -> ObjData -> ObjData
parseLines [] d = ObjData (L.reverse $ v d) (L.reverse $ n d) (L.reverse $ vecIndices d) (L.reverse $ normIndices d)
parseLines (x:xs) d = let tokens = lexer x in
                        case tokens of
                            [] -> parseLines xs d
                            t -> parseLines xs $ updateObjData (objParse t) d

updateObjData :: Value -> ObjData -> ObjData
updateObjData (ObjParser.Vertex3 x y z) (ObjData v n i ni) = ObjData (v3:v) n i ni
    where v3 = M.Vector3 x y z
updateObjData (ObjParser.FaceDef a1 a2 a3) (ObjData v n i ni) = ObjData v n (idx:i) ni
    where idx = TriangleIndex (vIdx a1 - 1) (vIdx a2 - 1) (vIdx a3 - 1)
updateObjData _ m = m
