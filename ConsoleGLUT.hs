module ConsoleGLUT where

import Data.Char
import Control.Monad
import Control.Concurrent (threadDelay)
import Control.Applicative
import Graphics.Rendering.OpenGL (($=), GLfloat, GLint)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.GLUtil as GLU

charWidth :: GLfloat
charWidth = 16.0

charHeight :: GLfloat
charHeight = 16.0

data Console = Console

-- | Loads texture and sets filtering and wrapping modes
loadTexture :: FilePath -> IO GL.TextureObject
loadTexture texturePath = do
    tex <- either error id <$> GLU.readTexture texturePath
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Nearest)
    GLU.texture2DWrap $= (GL.Mirrored, GL.ClampToEdge)
    return tex


-- | Draws a single character
drawChar :: Int         -- ^ ASCII code of the character
         -> (Int, Int)  -- ^ Position on the console
         -> IO ()
drawChar charCode (x', y') = GL.renderPrimitive GL.Quads . vertexInfo $ quad
    where
        vertexInfo :: [(GLfloat,GLfloat,GLfloat,GLfloat)] -> IO ()
        vertexInfo = mapM_ (\(xx,yy,u,v) -> do
            GL.color (GL.Color3 1.0 0.0 (0.0 :: GLfloat))
            GL.texCoord (GL.TexCoord2 u v)
            GL.vertex (GL.Vertex3 xx yy 0.0))

        cw = charWidth
        ch = charHeight
        (x, y) = (fromIntegral x' * charWidth, fromIntegral y' * charHeight)
        texmapW = charWidth * 16.0
        texmapH = charHeight * 16.0
        texX = fromIntegral (charCode `mod` 16) * charWidth
        texY = fromIntegral (charCode `quot` 16) * charHeight

        quad :: [(GLfloat,GLfloat,GLfloat,GLfloat)]
        quad = [(x,    y,    texX / texmapW,      texY / texmapH),
                (x+cw, y,    (texX+cw) / texmapW, texY / texmapH),
                (x+cw, y+ch, (texX+cw) / texmapW, (texY+ch) / texmapH),
                (x,    y+ch, texX / texmapW,      (texY+ch) / texmapH)]


-- | Draws a string on the console
drawString :: String      -- ^ String to draw
           -> (Int, Int)  -- ^ Position on the console
           -> IO ()
drawString [] _ = return ()
drawString (c:hars) (x, y) = drawChar (ord c) (x, y) >> drawString hars (x+1, y)


-- | Clears the console
clearConsole :: IO ()
clearConsole = GL.clear [GL.ColorBuffer, GL.DepthBuffer] >> GL.loadIdentity

-- | Flushes the console updates to the screen
flushConsole :: Console -> IO ()
flushConsole con = GL.flush


consoleShouldClose :: Console -> IO Bool
consoleShouldClose = const $ return False

runConsole :: GLint -> GLint -> String -> IO () -> IO ()
runConsole width height title action = do
    _ <- GLUT.getArgsAndInitialize
    _ <- GLUT.createWindow title
    GLUT.windowSize $= GLUT.Size width height
    GLUT.displayCallback $= action

    GL.viewport $= (GL.Position 0 0, GL.Size windowWidth windowHeight)
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D 0.0 (fromIntegral windowWidth) (fromIntegral windowHeight) 0.0
    GL.matrixMode $= GL.Modelview 0

    tex <- loadTexture "terminal16x16.png"
    GL.texture GL.Texture2D $= GL.Enabled
    GL.activeTexture $= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D $= Just tex

    GLUT.mainLoop
    where
        windowWidth :: GLint
        windowWidth = width * truncate charWidth

        windowHeight :: GLint
        windowHeight = height * truncate charHeight