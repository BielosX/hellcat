module ObjFile where

import ObjLexer
import ObjParser
import Model as M

import Data.Text.Lazy.IO as I
import Data.Text.Lazy as T
import System.IO
import Data.List as L
import Data.Vector
import Control.Monad.State.Lazy
import Data.Maybe

data ObjData = ObjData {
    v :: [M.Vector3],
    n :: [M.Vector3],
    vecIndices :: [TriangleIndex],
    normIndices :: [TriangleIndex]
}

emptyData = ObjData [] [] [] []

convert :: Vector M.Vector3 -> Vector M.Vector3 -> [TriangleIndex] -> [TriangleIndex] -> ([M.Vector3], [M.Vector3])
convert vert norm [] [] = ([], [])
convert vert norm ((TriangleIndex v1 v2 v3):xs) ((TriangleIndex n1 n2 n3):ys) = let (v, n) = ObjFile.convert vert norm xs ys in (cv L.++ v, cn L.++ n)
    where cv = fmap (\v -> vert ! v) [v1,v2,v3]
          cn = fmap (\v -> norm ! v) [n1,n2,n3]

readObjFile :: FilePath -> IO Model
readObjFile path = do
    content <- I.readFile path
    let d = parseLines (fmap T.unpack $ T.lines content) emptyData
    let c = ObjFile.convert (fromList $ v d) (fromList $ n d) (vecIndices d) (normIndices d)
    return $ M.Model (fst c) (snd c) []

parseLines :: [String] -> ObjData -> ObjData
parseLines [] d = ObjData (L.reverse $ v d)
    (L.reverse $ n d)
    (L.reverse $ vecIndices d)
    (L.reverse $ normIndices d)
parseLines (x:xs) d = let tokens = lexer x in
                        case tokens of
                            [] -> parseLines xs d
                            t -> parseLines xs $ updateObjData (objParse t) d

updateObjData :: Value -> ObjData -> ObjData
updateObjData (ObjParser.Vertex3 x y z) (ObjData v n i ni) = ObjData (v3:v) n i ni
    where v3 = M.Vector3 x y z
updateObjData (ObjParser.NormVector x y z) (ObjData v n i ni) = ObjData v (n3:n) i ni
    where n3 = M.Vector3 x y z
updateObjData (ObjParser.FaceDef a1 a2 a3) (ObjData v n i ni) = ObjData v n (idx:i) (maybe ni (\ti -> ti:ni) nIdx)
    where idx = TriangleIndex (vIdx a1 - 1) (vIdx a2 - 1) (vIdx a3 - 1)
          nIdx = toIdx $ fmap (\val -> val - 1) $ catMaybes [normIdx a1, normIdx a2, normIdx a3]
          toIdx [v1, v2, v3] = Just $ TriangleIndex v1 v2 v3
          toIdx _ = Nothing
updateObjData _ m = m
