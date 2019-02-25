module ObjFile where

import ObjLexer
import ObjParser
import Model as M

import Data.Text.Lazy.IO as I
import Data.Text.Lazy as T
import System.IO
import Data.List as L
import qualified Data.Map.Strict as Map
import Data.Vector
import Control.Monad.State.Lazy
import Data.Maybe

data ObjData = ObjData {
    v :: [M.Vector3],
    n :: [M.Vector3],
    vecIndices :: [TriangleIndex],
    normIndices :: [TriangleIndex]
}

data MapEntry = MapEntry {
    normalIdx :: Maybe Int,
    vec :: M.Vector3,
    norm :: Maybe M.Vector3
} deriving (Eq,Show)

data LookupTables = LookupTables {
    vVec :: Vector M.Vector3,
    nVec :: Vector M.Vector3
}

type IndicesMap = Map.Map Int MapEntry

emptyData = ObjData [] [] [] []

withNoNormIdx :: TriangleIndex -> LookupTables -> IndicesMap -> IndicesMap
withNoNormIdx (TriangleIndex x y z) t m = L.foldr (\a b -> Map.insert a (entry $ vert a) b) Map.empty [x,y,z]
    where vert i = (vVec t) ! i
          entry v = MapEntry Nothing v Nothing

newEntryWithNorm :: (Int,Int) -> LookupTables -> MapEntry
newEntryWithNorm (vIdx, nIdx) t = MapEntry (Just nIdx) vert (Just norm)
    where vert = (vVec t) ! vIdx
          norm = (nVec t) ! nIdx

idxPair :: Int -> (Int,Int) -> LookupTables -> IndicesMap -> ((Int, Int), IndicesMap)
idxPair next (vIdx, nIdx) t m = case Map.lookup vIdx m of
    Nothing -> ((next, vIdx), Map.insert vIdx (newEntryWithNorm (vIdx, nIdx) t) m)
    (Just k) -> if (fromMaybe (-1) $ normalIdx k) == nIdx then
                    ((next, vIdx), m)
                else
                    ((next+1, next), Map.insert next (newEntryWithNorm (vIdx, nIdx) t) m)

extract :: ((a,b), c) -> (a,b,c)
extract ((a,b), c) = (a,b,c)

withNormIdx :: Int ->
    TriangleIndex ->
    TriangleIndex ->
    LookupTables ->
    IndicesMap ->
    (Int, TriangleIndex, IndicesMap)
withNormIdx next (TriangleIndex x y z) (TriangleIndex nx ny nz) t m = extract $ (flip runState) m $ do
    (next1, vIdx1) <- state $ idxPair next (x,nx) t
    (next2, vIdx2) <- state $ idxPair next1 (y, ny) t
    (next3, vIdx3) <- state $ idxPair next2 (z, nz) t
    return (next3, TriangleIndex vIdx1 vIdx2 vIdx3)

_recalculate :: Int ->
    [(TriangleIndex, Maybe TriangleIndex)] ->
    LookupTables  ->
    IndicesMap ->
    ([TriangleIndex], IndicesMap)
_recalculate next [] t m = ([], m)
_recalculate next ((idx, Nothing):xs) t m = let (i, newMap) = _recalculate next xs t (withNoNormIdx idx t m) in (idx:i, newMap)
_recalculate next ((idx, Just nIdx):xs) t m = let (i, newMap) = _recalculate nxt xs t im in (newIdx:i, newMap)
    where (nxt, newIdx, im) = withNormIdx next idx nIdx t m

zipMaybe :: [a] -> [b] -> [(a, Maybe b)]
zipMaybe [] _ = []
zipMaybe (x:xs) [] = (x, Nothing):(zipMaybe xs [])
zipMaybe (x:xs) (y:ys) = (x, Just y):(zipMaybe xs ys)

extractVertices :: IndicesMap -> [M.Vector3]
extractVertices = fmap (vec . snd) . sortBy keys . Map.toList
    where keys = \(k1,v1) (k2,v2) -> compare k1 k2

extractNormals :: IndicesMap -> [M.Vector3]
extractNormals = catMaybes . fmap (norm . snd) . sortBy keys . Map.toList
    where keys = \(k1,v1) (k2,v2) -> compare k1 k2

recalculate :: ObjData -> ([M.Vector3], [M.Vector3], [TriangleIndex])
recalculate obj = let (idxs, m) =  _recalculate availableIdx pairs lookupTables Map.empty in (extractVertices m, extractNormals m, idxs)
    where availableIdx = (+1) $ L.maximum (vecIndices obj >>= \(TriangleIndex x y z) -> [x,y,z])
          pairs = zipMaybe (vecIndices obj) (normIndices obj)
          lookupTables = LookupTables (fromList $ v obj) (fromList $ n obj)

readObjFile :: FilePath -> IO Model
readObjFile path = do
    content <- I.readFile path
    let d = parseLines (fmap T.unpack $ T.lines content) emptyData
    let (vert, norm, ind) = recalculate d
    return $ M.Model vert ind

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
updateObjData (ObjParser.FaceDef a1 a2 a3) (ObjData v n i ni) = ObjData v n (idx:i) ni
    where idx = TriangleIndex (vIdx a1 - 1) (vIdx a2 - 1) (vIdx a3 - 1)
updateObjData _ m = m
