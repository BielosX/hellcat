module Texture(
                loadTexture,
                BufferedTexture(..),
                useTexture,
                loadImage,
                loadDynamicImage
                ) where

import qualified Data.ByteString as B

import Graphics.GL.Functions
import Graphics.GL.Groups
import Graphics.GL.Types
import Graphics.GL.Tokens
import System.FilePath.Posix
import Control.Monad.Except
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Codec.Picture.Types
import Codec.Picture.Png
import Codec.Picture.Extra

data BufferedTexture = BufferedTexture GLuint deriving (Eq, Show)

useTexture (BufferedTexture t) = glBindTexture GL_TEXTURE_2D t

storeAt :: Storable a => Ptr a -> [a] -> IO (Ptr a)
storeAt ptr [] = return ptr
storeAt ptr (x:xs) = do
    poke ptr x
    storeAt (plusPtr ptr $ sizeOf x) xs

storeTexture :: (Pixel a, Storable b) =>
    Image a ->
    Int ->
    (a -> [b]) ->
    GLenum ->
    GLenum ->
    GLenum ->
    IO BufferedTexture
storeTexture image elemSize toStorable intFormat format dataType = do
    id <- genTexture
    let flipImg = flipVertically image
    let width = (imageWidth flipImg)
    let height = (imageHeight flipImg)
    let glW = fromIntegral width :: GLsizei
    let glH = fromIntegral height :: GLsizei
    let bytes = width * height * elemSize
    let iFormat = fromIntegral intFormat :: GLint
    glBindTexture GL_TEXTURE_2D id
    allocaBytes bytes $ \ptr -> do
        pixelFoldM (\acc w h pixel -> storeAt acc $ toStorable pixel) ptr flipImg
        glTexImage2D GL_TEXTURE_2D 0 iFormat glW glH 0 format dataType ptr
    let nearest = fromIntegral GL_NEAREST :: GLint
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER nearest
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER nearest
    return $ BufferedTexture id

genTexture :: IO GLuint
genTexture = alloca $ \ptr -> do
    glGenTextures 1 ptr
    value <- peek ptr
    return value

loadImage :: DynamicImage -> ExceptT String IO BufferedTexture
loadImage dynamicImage = case dynamicImage of
        (ImageRGBA8 i) -> do
            let pixel = 0 :: Pixel8
            let size = (4*) $ sizeOf pixel
            liftIO $ storeTexture i size (\(PixelRGBA8 r g b a) -> [r,g,b,a]) GL_RGBA8 GL_RGBA GL_UNSIGNED_BYTE
        (ImageRGB8 i) -> do
            let pixel = 0 :: Pixel8
            let size = (3*) $ sizeOf pixel
            liftIO $ storeTexture i size (\(PixelRGB8 r g b) -> [r,g,b]) GL_RGB8 GL_RGB GL_UNSIGNED_BYTE
        _ -> throwError "format not supported"

loadPng :: FilePath -> ExceptT String IO DynamicImage
loadPng path = do
    content <- liftIO $ B.readFile path
    liftEither $ decodePng content

loadDynamicImage :: FilePath -> ExceptT String IO DynamicImage
loadDynamicImage path = case takeExtension path of
    ".png" -> loadPng path
    _ -> throwError "file format not supported"

loadTexture :: FilePath -> ExceptT String IO BufferedTexture
loadTexture path = do
    image <- loadDynamicImage path
    loadImage image
