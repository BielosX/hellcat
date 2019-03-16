{-# LANGUAGE DeriveGeneric #-}
module SceneDescription(loadScene, toCamera) where

import GHC.Generics
import System.FilePath.Posix
import Control.Monad.Except
import System.IO as S
import qualified Data.Aeson as Aeson
import Data.Aeson.Types
import qualified Data.Yaml as Yaml
import Data.ByteString.Lazy.Char8 as LC
import Data.ByteString.Char8 as C
import qualified Data.Map.Strict as Map
import Linear.Quaternion
import Linear.V3
import Linear.V4
import Linear.Matrix
import qualified Data.Set as Set
import Data.List as List
import Data.Bifunctor
import qualified Data.Text as T

import Scene
import BufferedObject
import Shader
import Wavefront.ObjFile
import SceneObject
import Config
import Camera
import Light

data ModelFile = WaveFront {
    file :: String
} deriving (Generic, Show, Eq, Ord)

instance Aeson.FromJSON ModelFile where

data Position = Position {
    xp :: Float,
    yp :: Float,
    zp :: Float
} deriving (Generic, Show)

instance Aeson.FromJSON Position where

toVector :: Position -> V3 Float
toVector (Position x y z) = V3 x y z

toVector4 (Position x y z) = V4 x y z 1

data Rotation = Rotation {
    xr :: Float,
    yr :: Float,
    zr :: Float
} deriving (Generic, Show)

toQuaternion :: Rotation -> Quaternion Float
toQuaternion (Rotation x y z) = zr * yr * xr
    where xr = axisAngle (V3 1 0 0) x
          yr = axisAngle (V3 0 1 0) y
          zr = axisAngle (V3 0 0 1) z

instance Aeson.FromJSON Rotation where

ratio :: Resolution -> Float
ratio (Resolution width height) = w / h
    where w = fromIntegral width :: Float
          h = fromIntegral height :: Float

data CameraDescription = CameraDescription {
    camId :: Int,
    camPosition :: Position,
    camRotation :: Rotation
} deriving (Generic, Show)

instance Aeson.FromJSON CameraDescription where

toCamera :: Resolution -> CameraDescription -> Camera
toCamera res (CameraDescription id pos rot) = perCam {view = trans}
    where perCam = perspectiveCam id (pi/4.0) (ratio res) 0.1 100.0
          p = toVector pos
          r = toQuaternion rot
          trans = mkTransformation r p

data SceneObjectDescription = SceneObjectDescription {
    objId :: Int,
    position :: Position,
    rotation :: Rotation,
    modelFile :: ModelFile,
    shaders :: [String]
} deriving (Generic, Show)

instance Aeson.FromJSON SceneObjectDescription where

data LightDescription = PointLightDescription {
    intensity :: Float,
    lightPosition :: Position
} deriving (Generic, Show)

instance Aeson.FromJSON LightDescription where
    parseJSON = withObject "LightDescription" $ \v -> do
        lightType <- parseField v $ T.pack "type"
        case lightType of
            "PointLight" -> do
                i <- parseField v (T.pack "intensity") :: Parser Float
                p <- parseField v (T.pack "position") :: Parser Position
                return $ PointLightDescription i p
            _ -> fail "Wrong light type"

data SceneDescription = SceneDescription {
    cameras :: [CameraDescription],
    objects :: [SceneObjectDescription],
    lights :: [LightDescription]
} deriving (Generic, Show)

instance Aeson.FromJSON SceneDescription where

yamlError :: Yaml.ParseException -> String
yamlError (Yaml.AesonException s) = s
yamlError e = show e

type BoMap = Map.Map ModelFile BufferedObject
type ShaderMap = Map.Map String Shader
type ProgramMap = Map.Map (Set.Set String) Program

getBufferedObject :: ModelFile -> BoMap -> IO (BoMap, BufferedObject)
getBufferedObject mf bm = case Map.lookup mf bm of
    Nothing -> do
        let path = file mf
        model <- readObjFile path
        bo <- loadModel model
        return (Map.insert mf bo bm, bo)
    (Just bo) -> return (bm, bo)

getType :: String -> ShaderType
getType s = case takeExtension s of
    ".frag" -> FragmentShader
    ".vert" -> VertexShader

getShader :: String -> ShaderMap -> ExceptT String IO (ShaderMap, Shader)
getShader shader sm = case Map.lookup shader sm of
    (Just s) -> return (sm, s)
    Nothing -> do
        compiled <- loadShader (getType shader) shader
        return (Map.insert shader compiled sm, compiled)

getProgram :: [String] -> ShaderMap -> ProgramMap -> ExceptT String IO (ShaderMap, ProgramMap, Program)
getProgram shaders sm pm = case Map.lookup (Set.fromList shaders) pm of
    (Just p) -> return (sm, pm, p)
    Nothing -> do
        mapsAndShaders <- mapM (\s -> getShader s sm) shaders
        let newSM = List.foldr Map.union Map.empty $ fmap fst mapsAndShaders
        program <- createProgram $ fmap snd mapsAndShaders
        let shadersSet = Set.fromList shaders
        return (newSM, Map.insert shadersSet program pm, program)

loadSceneObjects d = _loadSceneObjects d Map.empty Map.empty Map.empty

_loadSceneObjects :: [SceneObjectDescription] -> BoMap -> ShaderMap -> ProgramMap -> ExceptT String IO [SceneObject]
_loadSceneObjects [] _ _ _ = return []
_loadSceneObjects (x:xs) bm sm pm = do
    let s = shaders x
    (newSM, newPM, program) <- getProgram s sm pm
    (newBM, bo) <- lift $ getBufferedObject (modelFile x) bm
    let pos = toVector $ SceneDescription.position x
    let rot = toQuaternion $ rotation x
    let transMat = mkTransformation rot pos
    let sceneObject = SceneObject (objId x) bo identity program Nothing
    fmap (sceneObject:) (_loadSceneObjects xs newBM newSM newPM)

toLight :: LightDescription -> Light
toLight (PointLightDescription i p) = PointLight i (toVector4 p)

loadSceneLights :: [LightDescription] -> ExceptT String IO Lights
loadSceneLights [] = return $ Lights [] emptySSB emptySSB
loadSceneLights a = do
    let l = fmap toLight a
    let inten = fmap Light.intensity l
    let pos = fmap Light.position l
    posSSB <- liftIO $ loadShaderStorageBuffer pos (\(V4 x y z w) -> [x,y,z,w])
    intenSSB <- liftIO $ loadShaderStorageBuffer inten (\i -> [i])
    return $ Lights l posSSB intenSSB

getSceneDescription :: FilePath -> ExceptT String IO SceneDescription
getSceneDescription path = do
    let ext = takeExtension path
    content <- liftIO $ S.readFile path
    case ext of
        ".yaml" -> do
                s <- liftEither $ first SceneDescription.yamlError $ Yaml.decodeEither' $ C.pack content
                return s
        ".json" -> do
                s <- liftEither $ Aeson.eitherDecode' $ LC.pack content
                return s
        _ -> throwError "file type not supported"

loadScene :: FilePath -> (CameraDescription -> Camera) -> ExceptT String IO Scene
loadScene path f= do
    description <- getSceneDescription path
    sceneObjects <- loadSceneObjects (objects description)
    l <- loadSceneLights (SceneDescription.lights description)
    let cam = fmap f (cameras description)
    result <- liftEither $ scene sceneObjects cam l
    return result

