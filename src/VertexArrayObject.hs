module VertexArrayObject (loadModel, drawVAO, VertexArrayObject(..)) where

import Model

import Graphics.GL.Functions
import Graphics.GL.Groups
import Graphics.GL.Types
import Graphics.GL.Tokens
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr
import Control.Monad

data VertexArrayObject = VertexArrayObject {
    id :: GLuint,
    indicesCount :: GLsizei,
    verticesCount :: GLsizei,
    idxBuffer :: GLuint
}

newtype VertexBuffer = VertexBuffer GLuint

newtype IndexBuffer = IndexBuffer GLuint

data Vertex3 = Vertex3 GLfloat GLfloat GLfloat

data Index = Index GLuint GLuint GLuint

vertexCoordIdx = 0

convV3 :: Model.Vertex3 -> VertexArrayObject.Vertex3
convV3 (Model.Vertex3 x y z) = VertexArrayObject.Vertex3 x y z

convIdx :: TriangleIndex -> Index
convIdx (TriangleIndex x y z) = Index (c x) (c y) (c z)
    where c = (fromIntegral :: Int -> GLuint)

loadModel :: Model -> IO VertexArrayObject
loadModel (Model v i) = do
    vao <- newVAO
    vbo <- newVertexBuffer $ fmap convV3 v
    assignVertexBufferToVAO vbo vao
    let iLength =  length i
    let vLen = (fromIntegral :: Int -> GLsizei) $ length v
    if iLength > 0 then do
        (IndexBuffer idxBuff) <- newIndexBuffer $ fmap convIdx i
        let count = (fromIntegral :: Int -> GLsizei) iLength * 3
        return $ vao {indicesCount = count, idxBuffer = idxBuff, verticesCount = vLen}
    else return $ vao { verticesCount = vLen }

drawVAO :: VertexArrayObject -> IO ()
drawVAO v@(VertexArrayObject id iCount vCount idxBuff) = do
    setCurrentVAO v
    if iCount > 0 then do
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER idxBuff
        glDrawElements GL_TRIANGLES iCount GL_UNSIGNED_INT nullPtr
    else
        glDrawArrays GL_TRIANGLES 0 vCount

newVAO :: IO VertexArrayObject
newVAO = alloca $ \ptr -> do
            glGenVertexArrays 1 ptr
            id <- peek ptr
            return $ VertexArrayObject id 0 0 0

setCurrentVAO :: VertexArrayObject -> IO ()
setCurrentVAO (VertexArrayObject id _ _ _) = glBindVertexArray id

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

newVertexBuffer :: [VertexArrayObject.Vertex3] -> IO VertexBuffer
newVertexBuffer v = fmap VertexBuffer $ newBuffer v (\(VertexArrayObject.Vertex3 x y z) -> [x,y,z]) GL_ARRAY_BUFFER

newIndexBuffer :: [Index] -> IO IndexBuffer
newIndexBuffer i = fmap IndexBuffer $ newBuffer i (\(Index x y z) -> [x,y,z]) GL_ELEMENT_ARRAY_BUFFER

assignVertexBufferToVAO :: VertexBuffer -> VertexArrayObject -> IO ()
assignVertexBufferToVAO (VertexBuffer vb) (VertexArrayObject vao _ _ _) = do
    glBindVertexArray vao
    glEnableVertexAttribArray vertexCoordIdx
    glBindBuffer GL_ARRAY_BUFFER vb
    glVertexAttribPointer vertexCoordIdx 3 GL_FLOAT 0 0 nullPtr
