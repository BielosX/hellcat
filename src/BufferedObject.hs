module BufferedObject (loadModel, drawObject, BufferedObject(..)) where

import Model
import Buffer

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
    id :: GLuint
}

data VertexBuffer = VertexBuffer {
    vSize :: GLsizei,
    vId :: GLuint
}

data IndexBuffer = IndexBuffer {
    iSize :: GLsizei,
    iId :: GLuint
}

data BufferedObject = BufferedObject {
    vao :: VertexArrayObject,
    vertices :: VertexBuffer,
    indices :: Maybe IndexBuffer
}

data Vertex3 = Vertex3 GLfloat GLfloat GLfloat

data FaceIndices = FaceIndices GLuint GLuint GLuint

vertexCoordIdx = 0

convV3 :: Model.Vertex3 -> BufferedObject.Vertex3
convV3 (Model.Vertex3 x y z) = BufferedObject.Vertex3 x y z

convIdx :: TriangleIndex -> FaceIndices
convIdx (TriangleIndex x y z) = FaceIndices (c x) (c y) (c z)
    where c = (fromIntegral :: Int -> GLuint)

loadModel :: Model -> IO BufferedObject
loadModel (Model v i) = do
    vao <- newVAO
    vbo <- newVertexBuffer $ fmap convV3 v
    assignVertexBufferToVAO vbo vao
    if length i > 0 then do
        idxbuff <- newIndexBuffer $ fmap convIdx i
        return $ BufferedObject vao vbo (Just idxbuff)
    else return $ BufferedObject vao vbo Nothing

drawObject :: BufferedObject -> IO ()
drawObject b = do
    setCurrentVAO $ vao b
    let idxs = BufferedObject.indices b
    let vLen = vSize $ BufferedObject.vertices b
    case idxs of
        (Just i) -> do
            let iLen = iSize i
            glBindBuffer GL_ELEMENT_ARRAY_BUFFER (iId i)
            glDrawElements GL_TRIANGLES iLen GL_UNSIGNED_INT nullPtr
        Nothing -> glDrawArrays GL_TRIANGLES 0 vLen

newVAO :: IO VertexArrayObject
newVAO = alloca $ \ptr -> do
            glGenVertexArrays 1 ptr
            id <- peek ptr
            return $ VertexArrayObject id

setCurrentVAO :: VertexArrayObject -> IO ()
setCurrentVAO (VertexArrayObject id) = glBindVertexArray id

newVertexBuffer :: [BufferedObject.Vertex3] -> IO VertexBuffer
newVertexBuffer v = fmap (VertexBuffer size) $ newBuffer v (\(BufferedObject.Vertex3 x y z) -> [x,y,z]) GL_ARRAY_BUFFER
    where size = (fromIntegral :: Int -> GLsizei) $ length v

newIndexBuffer :: [FaceIndices] -> IO IndexBuffer
newIndexBuffer i = fmap (IndexBuffer size) $ newBuffer i (\(FaceIndices x y z) -> [x,y,z]) GL_ELEMENT_ARRAY_BUFFER
    where size = (fromIntegral :: Int -> GLsizei) $ (length i) * 3

assignVertexBufferToVAO :: VertexBuffer -> VertexArrayObject -> IO ()
assignVertexBufferToVAO (VertexBuffer _ vb) (VertexArrayObject vao) = do
    glBindVertexArray vao
    glEnableVertexAttribArray vertexCoordIdx
    glBindBuffer GL_ARRAY_BUFFER vb
    glVertexAttribPointer vertexCoordIdx 3 GL_FLOAT 0 0 nullPtr
