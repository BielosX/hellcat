{
module Wavefront.MtlParser where

import Wavefront.MtlLexer
}

%name mtlParse
%tokentype { Token }
%error { parseError }

%token
    map_Kd { DiffuseTexture }
    path { FilePath $$ }

%%

Exp : map_Kd path { DiffTexture $2 }

{

data Value = DiffTexture String

parseError :: [Token] -> a
parseError t = error $ "Parse error, tokens: " ++ (show t)

}
