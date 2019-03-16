{
module Wavefront.ObjLexer where

import Data.List
}

%wrapper "basic"

$digit = [0-9]

tokens :-
    "s".* ;
    "mtllib".* ;
    "usemtl".* ;
    "o".* ;
    "#".*   ;
    $white+ ;
    vt                      { \s -> TextureCoord }
    vn                      { \s -> NormVect }
    v                       { \s -> Vertex }
    f                       { \s -> Face }
    "-"?$digit+\.$digit+        { \s -> FloatValue (read s :: Float) }
    ($digit+ | $digit+\/$digit+ | $digit+\/$digit+\/$digit+ | $digit+\/\/$digit+) { FaceIdx }
{

data Token = FloatValue Float |
             Vertex |
             Face |
             TextureCoord |
             FaceIdx String |
             NormVect deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}
