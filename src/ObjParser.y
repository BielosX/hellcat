{
module ObjParser where

import ObjLexer
import Data.Maybe
import Data.List
import Data.List.Split
}

%name objParse
%tokentype { Token }
%error { parseError }

%token
    vertex { Vertex }
    float { FloatValue $$ }
    face { Face }
    texCoord { TextureCoord }
    faceDef { FaceIdx $$ }
    normVec { NormVect }

%%


Exp : vertex float float float { Vertex3 $2 $3 $4 }
      | normVec float float float { NormVector $2 $3 $4 }
      | face faceDef faceDef faceDef { FaceDef (parseFaceDef $2) (parseFaceDef $3) (parseFaceDef $4) }
      | texCoord float float { TexCoord $2 $3 }
{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Value = Vertex3 Float Float Float |
             TexCoord Float Float |
             NormVector Float Float Float |
             FaceDef FaceD FaceD FaceD deriving (Eq, Show)

readI :: String -> Int
readI = read

parseFaceDef :: String -> FaceD
parseFaceDef = _parseFaceDef . splitOn "/"

_parseFaceDef :: [String] -> FaceD
_parseFaceDef [x] = FaceD (readI x) Nothing Nothing
_parseFaceDef [x,y] = FaceD (readI x) (Just $ readI y) Nothing
_parseFaceDef [x,y,z] | null y = FaceD (readI x) Nothing (Just $ readI z)
                     | otherwise = FaceD (readI x) (Just $ readI y) (Just $ readI z)

data FaceD = FaceD {
    vIdx :: Int,
    texIdx :: Maybe Int,
    normIdx :: Maybe Int
} deriving (Eq, Show)

}
