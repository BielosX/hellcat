module Lib
    ( someFunc
    ) where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Functions
import Graphics.GL.Groups
import Graphics.GL.Types
import Graphics.GL.Tokens
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr
import Control.Monad

data VertexArrayObject = VertexArrayObject GLuint

data VertexBuffer = VertexBuffer GLuint Int

data Vertex3 = Vertex3 GLfloat GLfloat GLfloat

type AttrIndex = Int

newVAO :: IO VertexArrayObject
newVAO = alloca $ \ptr -> do
            glGenVertexArrays 1 ptr
            id <- peek ptr
            return $ VertexArrayObject id

setCurrentVAO :: VertexArrayObject -> IO ()
setCurrentVAO (VertexArrayObject id) = glBindVertexArray id

newVertexBuffer :: [Vertex3] -> IO VertexBuffer
newVertexBuffer v = alloca $ \ptr -> do
            glGenBuffers 1 ptr
            id <- peek ptr
            glBindBuffer GL_ARRAY_BUFFER id
            let vertices = join $ fmap (\(Vertex3 x y z) -> [x,y,z]) v
            let float = 0 :: GLfloat
            let size = (length vertices) * (sizeOf float)
            withArray vertices $ \arrayPtr -> do
                glBufferData GL_ARRAY_BUFFER (fromIntegral size :: GLsizeiptr) arrayPtr GL_STATIC_DRAW
            return $ VertexBuffer id (length v)

assignVertexBufferToVAO :: AttrIndex -> VertexBuffer -> VertexArrayObject -> IO ()
assignVertexBufferToVAO idx (VertexBuffer vb len) (VertexArrayObject vao) = do
    glBindVertexArray vao
    let index = (fromIntegral idx :: GLuint)
    glEnableVertexAttribArray index
    glBindBuffer GL_ARRAY_BUFFER vb
    glVertexAttribPointer index (fromIntegral len :: GLint) GL_FLOAT 0 0 nullPtr

render :: GLFW.Window -> [VertexArrayObject] -> Int -> IO ()
render window vao i = do
    close <- GLFW.windowShouldClose window
    if close then do
        GLFW.destroyWindow window
        GLFW.terminate
        return ()
    else do
        glClear GL_COLOR_BUFFER_BIT
        GLFW.pollEvents
        k0 <- GLFW.getKey window GLFW.Key'A
        k1 <- GLFW.getKey window GLFW.Key'S
        setCurrentVAO (vao !! i)
        glDrawArrays GL_TRIANGLES 0 3
        GLFW.swapBuffers window
        case k0 of
            GLFW.KeyState'Pressed -> render window vao 0
            _ -> case k1 of
                    GLFW.KeyState'Pressed -> render window vao 1
                    _ -> render window vao i

triangle = [Vertex3 0 0 0, Vertex3 0 1 0, Vertex3 1 0 0]
triangle2 = [Vertex3 0 0 0, Vertex3 1 1 0, Vertex3 1 0 0]

someFunc :: IO ()
someFunc = do
    r <- GLFW.init
    if not r then do
        putStrLn "Unable to initialize GLFW"
        return ()
    else do
        w <- GLFW.createWindow 800 600 "Test" Nothing Nothing
        case w of
            (Just window) -> do
                GLFW.makeContextCurrent (Just window)
                GLFW.setStickyKeysInputMode window GLFW.StickyKeysInputMode'Enabled
                vao <- newVAO
                vb <- newVertexBuffer triangle
                assignVertexBufferToVAO 0 vb vao
                vao2 <- newVAO
                vb2 <- newVertexBuffer triangle2
                assignVertexBufferToVAO 0 vb2 vao2
                render window [vao, vao2] 0
            Nothing -> do
                putStrLn "Unable to create window"
                GLFW.terminate
