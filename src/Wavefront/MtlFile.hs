module Wavefront.MtlFile(readMtlFile) where

import Data.Text.Lazy.IO as I
import Data.Text.Lazy as T
import System.IO

import Wavefront.MtlLexer
import Wavefront.MtlParser
import Material

data MtlData = MtlData {
    diffuseTexturePath :: String
}

readMtlFile :: FilePath -> IO MaterialInfo
readMtlFile path  = do
    content <- I.readFile path
    let d = parseLines (fmap T.unpack $ T.lines content) $ MtlData []
    return $ MaterialInfo $ Wavefront.MtlFile.diffuseTexturePath d

parseLines :: [String] -> MtlData -> MtlData
parseLines [] mtl = mtl
parseLines (x:xs) mtl = let tokens = lexer x in
                            case tokens of
                            [] -> parseLines xs mtl
                            t -> parseLines xs $ updateMtlData (mtlParse t) mtl

updateMtlData :: Value -> MtlData -> MtlData
updateMtlData (DiffTexture t) mtl = MtlData t
