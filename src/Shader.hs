module Shader(
                loadShader,
                ShaderType(..),
                createProgram
            ) where

import System.IO
import Graphics.GL.Functions
import Graphics.GL.Groups
import Graphics.GL.Types
import Graphics.GL.Tokens
import System.IO.Strict as S
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import qualified Data.List as L
import Data.Char
import Control.Monad.Except
import Data.Either
import qualified Data.Map.Strict as Map
import Linear.V4
import Linear.Matrix

data ShaderType = VertexShader | FragmentShader deriving (Show,Eq)

data Status = Success | Fail

newtype Shader = Shader GLuint deriving (Show, Eq)

newtype Program = Program GLuint deriving (Show, Eq)

convType :: ShaderType -> GLenum
convType VertexShader = GL_VERTEX_SHADER
convType FragmentShader = GL_FRAGMENT_SHADER

glIntToShaderType = Map.fromList [(GL_VERTEX_SHADER, VertexShader), (GL_FRAGMENT_SHADER, FragmentShader)]

toGLcharList :: String -> [GLchar]
toGLcharList = fmap (\c -> (fromIntegral :: Int -> GLchar) $ ord c)

getParameter :: GLuint -> GLenum -> IO GLint
getParameter id q = alloca $ \ptr -> do
    glGetShaderiv id q ptr
    peek ptr

getShaderType :: GLuint -> IO ShaderType
getShaderType id = do
    value <- getParameter id GL_SHADER_TYPE
    return $ glIntToShaderType Map.! ((fromIntegral :: GLint -> GLenum) value)

getCompileStatus :: GLuint -> IO Status
getCompileStatus id = do
    value <- getParameter id GL_COMPILE_STATUS
    if value == 0 then
        return Fail
    else return Success

getSourceLen :: GLuint -> IO GLint
getSourceLen id = getParameter id GL_SHADER_SOURCE_LENGTH

getLogLen :: GLuint -> IO GLint
getLogLen id = alloca $ \ptr -> do
    glGetShaderiv id GL_INFO_LOG_LENGTH ptr
    value <- peek ptr
    return value

allocaGLcharArray :: Int -> (Ptr GLchar -> IO b) -> IO b
allocaGLcharArray = allocaArray

glCharArrayToString :: Ptr GLchar -> Int -> IO String
glCharArrayToString ptr 0 = return []
glCharArrayToString ptr n = do
    value <- peek ptr
    let c = chr $ (fromIntegral :: GLchar -> Int) value
    if c /= '\0' then do
        next <- glCharArrayToString (plusPtr ptr 1) (n-1)
        return $ c : next
    else glCharArrayToString ptr 0

getLog :: GLuint -> IO String
getLog id = do
    len <- getLogLen id
    let size = (fromIntegral :: GLint -> Int) len
    allocaGLcharArray size $ \ptr -> do
        glGetShaderInfoLog id len nullPtr ptr
        glCharArrayToString ptr size

loadShader :: ShaderType -> FilePath -> ExceptT String IO Shader
loadShader t p = do
    r <- lift $ _loadShader t p
    liftEither r

_loadShader :: ShaderType -> FilePath -> IO (Either String Shader)
_loadShader t p = do
    text <- S.readFile p
    shaderId <- glCreateShader $ convType t
    lenPtr <- malloc :: IO (Ptr GLint)
    poke lenPtr $ (fromIntegral :: Int -> GLint) $ length text
    withArray (toGLcharList text) $ \arrPtr -> do
        pp <- malloc :: IO (Ptr (Ptr GLchar))
        poke pp arrPtr
        glShaderSource shaderId 1 pp lenPtr
        free pp
    free lenPtr
    glCompileShader shaderId
    status <- getCompileStatus shaderId
    case status of
        Success -> return $ Right $ Shader shaderId
        Fail -> do
            log <- getLog shaderId
            glDeleteShader shaderId
            return $ Left log

_vectorToList :: V4 a -> [a]
_vectorToList (V4 x y z w) = [x,y,z,w]

uniformMatrix :: GLint -> M44 Float -> IO ()
uniformMatrix location matrix = do
    let arr = _vectorToList matrix >>= _vectorToList
    withArray arr $ glUniformMatrix4fv location 1 GL_TRUE

getLinkStatus :: GLuint -> IO Status
getLinkStatus id = alloca $ \ptr -> do
    glGetProgramiv id GL_LINK_STATUS ptr
    value <- peek ptr
    if value == 0 then return Fail
    else return Success

createProgram :: [Shader] -> ExceptT String IO Program
createProgram shaders = do
    r <- lift $ _createProgram shaders
    liftEither r

_createProgram :: [Shader] -> IO (Either String Program)
_createProgram shaders = do
    prog <- glCreateProgram
    mapM (\(Shader s) -> glAttachShader prog s) shaders
    glLinkProgram prog
    status <- getLinkStatus prog
    case status of
        Fail -> return $ Left "Unable to link program"
        Success -> return $ Right $ Program prog