{

module Wavefront.MtlLexer where

}

%wrapper "basic"

$alphaNum = [0-9a-zA-Z]

tokens :-
    "newmtl".* ;
    "Ns".* ;
    "Ka".* ;
    "Kd".* ;
    "Ks".* ;
    "Ke".* ;
    "Ni".* ;
    "d".* ;
    "illum".* ;
    "#".*   ;
    $white+ ;
    map_Kd  { \s -> DiffuseTexture }
    $alphaNum+\.$alphaNum+ { \s -> FilePath s }
{
data Token = DiffuseTexture |
             FilePath String deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}
