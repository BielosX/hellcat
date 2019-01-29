{
module ObjLexer where

import Data.Maybe
import Data.List
import Data.List.Split
}

%wrapper "basic"

$digit = [0-9]

tokens :-
    "s".* ;
    "mtllib".* ;
    "o".* ;
    "#".*   ;
    $white+ ;
    vt                      { \s -> TextureCoord }
    vn                      { \s -> NormVect }
    v                       { \s -> Vertex }
    f                       { \s -> Face }
    $digit+\.$digit+        { \s -> FloatValue (read s :: Float) }
    ($digit+ | $digit+\/$digit+ | $digit+\/$digit+\/$digit+ | $digit+\/\/$digit+) { parseIdx . splitOn "/" }
{

readI :: String -> Int
readI = read

parseIdx :: [String] -> Token
parseIdx [x] = FaceIdx (readI x) Nothing Nothing
parseIdx [x,y] = FaceIdx (readI x) (Just $ readI y) Nothing
parseIdx [x,y,z] | null y = FaceIdx (readI x) Nothing (Just $ readI z)
                 | otherwise = FaceIdx (readI x) (Just $ readI y) (Just $ readI z)


data Token = FloatValue Float |
             Vertex |
             Face |
             TextureCoord |
             FaceIdx Int (Maybe Int) (Maybe Int) |
             NormVect deriving (Eq, Show)
}
