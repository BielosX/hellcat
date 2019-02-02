module Shader(
                loadShader
            ) where

import System.IO
import Graphics.GL.Functions
import Graphics.GL.Groups
import Graphics.GL.Types
import Graphics.GL.Tokens
import Data.Text.Lazy.IO as I
import Data.Text.Lazy as Text
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import qualified Data.List as L
import Data.Char
import Control.Monad.Except
import Data.Either

data ShaderType = VertexShader | FragmentShader

data CompileStatus = Success | Fail

newtype Shader = Shader GLuint

convType :: ShaderType -> GLenum
convType VertexShader = GL_VERTEX_SHADER
convType FragmentShader = GL_FRAGMENT_SHADER

toGLcharList :: String -> [GLchar]
toGLcharList = fmap (\c -> (fromIntegral :: Int -> GLchar) $ ord c)

getCompileStatus :: GLuint -> IO CompileStatus
getCompileStatus id = alloca $ \ptr -> do
    glGetShaderiv id GL_COMPILE_STATUS ptr
    value <- peek ptr
    if value == 0 then
        return Fail
    else return Success

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
    text <- fmap Text.unpack $ I.readFile p
    shaderId <- glCreateShader $ convType t
    lenPtr <- malloc :: IO (Ptr GLint)
    poke lenPtr $ (fromIntegral :: Int -> GLint) $ L.length text
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
