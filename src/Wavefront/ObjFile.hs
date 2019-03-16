module Wavefront.ObjFile where

import Wavefront.ObjLexer
import Wavefront.ObjParser
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
    uv :: [M.Vector2],
    vecIndices :: [TriangleIndex],
    normIndices :: [TriangleIndex],
    uvIndices :: [TriangleIndex]
}

emptyData = ObjData [] [] [] [] [] []

data Coords = Coords {
    vCoord :: Vector M.Vector3,
    nCoord :: Vector M.Vector3,
    uvCoord :: Vector M.Vector2
} deriving (Eq, Show)

data Indices = Indices {
    vIndices :: [TriangleIndex],
    nIndices :: [TriangleIndex],
    uvInd :: [TriangleIndex]
}

firstI :: Indices -> Maybe (TriangleIndex, TriangleIndex, TriangleIndex)
firstI (Indices [] [] []) = Nothing
firstI (Indices (v:vs) (n:ns) (uv:uvs)) = Just (v,n,uv)

restI :: Indices -> Indices
restI i@(Indices [] [] []) = i
restI (Indices (v:vs) (n:ns) (uv:uvs)) = Indices vs ns uvs

getVCoord c ((TriangleIndex v1 v2 v3),_,_) = fmap (\v -> vert ! v) [v1, v2, v3]
    where vert = vCoord c

getNCoord c (_,(TriangleIndex n1 n2 n3),_) = fmap (\n -> norm ! n) [n1, n2, n3]
    where norm = nCoord c

getUVCoord c (_,_,(TriangleIndex uv1 uv2 uv3)) = fmap (\uv -> uvC ! uv) [uv1, uv2, uv3]
    where uvC = uvCoord c

convert :: Coords -> Indices -> ([M.Vector3], [M.Vector3], [M.Vector2])
convert coords indices = case firstI indices of
    Nothing -> ([], [], [])
    (Just i) -> let (v, n, uv) = Wavefront.ObjFile.convert coords (restI indices) in (vc L.++ v, nv L.++ n, uvc L.++ uv)
        where vc = getVCoord coords i
              nv = getNCoord coords i
              uvc = getUVCoord coords i

readObjFile :: FilePath -> IO Model
readObjFile path = do
    content <- I.readFile path
    let d = parseLines (fmap T.unpack $ T.lines content) emptyData
    let coords = Coords (fromList $ v d) (fromList $ n d) (fromList $ uv d)
    let indices = Indices (vecIndices d) (normIndices d) (uvIndices d)
    let (v, n, uv) = Wavefront.ObjFile.convert coords indices
    return $ M.Model v n [] uv

parseLines :: [String] -> ObjData -> ObjData
parseLines [] d = ObjData (L.reverse $ v d)
    (L.reverse $ n d)
    (L.reverse $ uv d)
    (L.reverse $ vecIndices d)
    (L.reverse $ normIndices d)
    (L.reverse $ uvIndices d)
parseLines (x:xs) d = let tokens = lexer x in
                        case tokens of
                            [] -> parseLines xs d
                            t -> parseLines xs $ updateObjData (objParse t) d

updateObjData :: Value -> ObjData -> ObjData
updateObjData (Wavefront.ObjParser.Vertex3 x y z) (ObjData v n uv i ni uvi) = ObjData (v3:v) n uv i ni uvi
    where v3 = M.Vector3 x y z
updateObjData (Wavefront.ObjParser.NormVector x y z) (ObjData v n uv i ni uvi) = ObjData v (n3:n) uv i ni uvi
    where n3 = M.Vector3 x y z
updateObjData (Wavefront.ObjParser.TexCoord x y) (ObjData v n uv i ni uvi) = ObjData v n (uv2:uv) i ni uvi
    where uv2 = M.Vector2 x y
updateObjData (Wavefront.ObjParser.FaceDef a1 a2 a3) (ObjData v n uv i ni uvi) = ObjData v n uv (idx:i) maybeNidx maybeUVidx
    where idx = TriangleIndex (vIdx a1 - 1) (vIdx a2 - 1) (vIdx a3 - 1)
          nIdx = toIdx $ fmap (\val -> val - 1) $ catMaybes [normIdx a1, normIdx a2, normIdx a3]
          uvIdx = toIdx $ fmap (\val -> val - 1) $ catMaybes [texIdx a1, texIdx a2, texIdx a3]
          toIdx [v1, v2, v3] = Just $ TriangleIndex v1 v2 v3
          toIdx _ = Nothing
          maybeNidx = maybe ni (\ti -> ti:ni) nIdx
          maybeUVidx = maybe uvi (\ti -> ti:uvi) uvIdx

