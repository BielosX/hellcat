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

data ArrayBuffer = ArrayBuffer {
    vSize :: GLsizei,
    attrType :: AttributeType,
    vId :: GLuint
}

data IndexBuffer = IndexBuffer {
    iSize :: GLsizei,
    iId :: GLuint
}

data UVBuffer = UVBuffer {
    uvSize :: GLsizei,
    uvId :: GLuint
}

data BufferedObject = BufferedObject {
    vao :: VertexArrayObject,
    vertices :: ArrayBuffer,
    indices :: Maybe IndexBuffer
}

data AttributeType = VeretexCoord | NormalCoord | UVCoord

data FaceIndices = FaceIndices GLuint GLuint GLuint

data VertexAttrib = Vertex3 GLfloat GLfloat GLfloat |
                    UVcoord GLfloat GLfloat

vertexCoordIdx = 0
normalsIdx = 1
uvIdx = 2

convV3 ::Vector3 -> VertexAttrib
convV3 (Vector3 x y z) = Vertex3 x y z

convV2 :: Vector2 -> VertexAttrib
convV2 (Vector2 x y) = UVcoord x y

convIdx :: TriangleIndex -> FaceIndices
convIdx (TriangleIndex x y z) = FaceIndices (c x) (c y) (c z)
    where c = (fromIntegral :: Int -> GLuint)

loadModel :: Model -> IO BufferedObject
loadModel (Model v n i uv) = do
    vao <- newVAO
    vbo <- newArrayBuffer VeretexCoord $ fmap convV3 v
    nVbo <- newArrayBuffer NormalCoord $ fmap convV3 n
    uvVbo <- newArrayBuffer UVCoord $ fmap convV2 uv
    assignArrayBufferToVAO vertexCoordIdx vbo vao
    assignArrayBufferToVAO normalsIdx nVbo vao
    assignArrayBufferToVAO uvIdx uvVbo vao
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

convFunc VeretexCoord = \(BufferedObject.Vertex3 x y z) -> [x,y,z]
convFunc NormalCoord = \(BufferedObject.Vertex3 x y z) -> [x,y,z]
convFunc UVCoord = \(UVcoord u v) -> [u,v]

newArrayBuffer :: AttributeType -> [VertexAttrib] -> IO ArrayBuffer
newArrayBuffer t v = fmap (ArrayBuffer size t) $ newBuffer v (convFunc t) GL_ARRAY_BUFFER
    where size = (fromIntegral :: Int -> GLsizei) $ length v

newIndexBuffer :: [FaceIndices] -> IO IndexBuffer
newIndexBuffer i = fmap (IndexBuffer size) $ newBuffer i (\(FaceIndices x y z) -> [x,y,z]) GL_ELEMENT_ARRAY_BUFFER
    where size = (fromIntegral :: Int -> GLsizei) $ (length i) * 3

attrSize VeretexCoord = 3
attrSize NormalCoord = 3
attrSize UVCoord = 2

assignArrayBufferToVAO :: GLuint -> ArrayBuffer -> VertexArrayObject -> IO ()
assignArrayBufferToVAO idx (ArrayBuffer s t vb) (VertexArrayObject vao) = do
    glBindVertexArray vao
    glEnableVertexAttribArray idx
    glBindBuffer GL_ARRAY_BUFFER vb
    glVertexAttribPointer idx (attrSize t) GL_FLOAT 0 0 nullPtr
