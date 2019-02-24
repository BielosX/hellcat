module Buffer where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Graphics.GL.Functions
import Graphics.GL.Groups
import Graphics.GL.Types
import Graphics.GL.Tokens
import Control.Monad

newBuffer :: Storable b => [a] -> (a -> [b]) -> GLenum -> IO GLuint
newBuffer [] _ _ = error "Array is empty"
newBuffer a f e = alloca $ \ptr -> do
            glGenBuffers 1 ptr
            id <- peek ptr
            glBindBuffer e id
            let array = join $ fmap f a
            let size = (length array) * (sizeOf $ head array)
            withArray array $ \arrayPtr -> do
                glBufferData e (fromIntegral size :: GLsizeiptr) arrayPtr GL_STATIC_DRAW
            return id

