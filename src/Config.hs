{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Config where

import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import Data.Maybe
import Data.Aeson.Types
import System.IO as S
import Data.ByteString.Lazy.Char8 as LC
import Data.ByteString.Char8 as C
import GHC.Generics
import Control.Monad.Except
import System.FilePath.Posix
import Data.Bifunctor

data Resolution = Resolution {
    width :: Int,
    height :: Int
} deriving (Generic, Show)

instance Aeson.ToJSON Resolution where
instance Aeson.FromJSON Resolution where

data Config = Config {
    resolution :: Resolution
} deriving (Generic, Show)

instance Aeson.ToJSON Config where

yamlError :: Yaml.ParseException -> String
yamlError (Yaml.AesonException s) = s
yamlError e = show e

instance Aeson.FromJSON Config where
    parseJSON = withObject "Config" $ \v -> do
        maybeResolution <- parseFieldMaybe v "resolution"
        resolution <- maybe (fail "resolution field should be provided") return maybeResolution
        return $ Config resolution

readConfigFile :: FilePath -> ExceptT String IO Config
readConfigFile path = do
    let extension = takeExtension path
    file <- liftIO $ S.readFile path
    case extension of
        ".yaml" -> do
                config <- liftEither $ first yamlError $ Yaml.decodeEither' $ C.pack file
                return config
        ".json" -> do
                config <- liftEither $ Aeson.eitherDecode' $ LC.pack file
                return config
        _ -> throwError "Unsupported file extension"
