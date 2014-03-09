module Console where

import Data.Char
import Control.Monad
import Control.Exception (finally)
import Control.Concurrent (threadDelay)
import Control.Applicative
import Graphics.Rendering.OpenGL (($=), GLfloat, GLint)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.GLUtil as GLU

charWidth :: GLfloat
charWidth = 8.0

charHeight :: GLfloat
charHeight = 8.0

type Console = GLFW.Window

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
flushConsole con = do
    GLFW.swapBuffers con
    GL.flush
    GLFW.pollEvents
    threadDelay 20000 -- small delay to prevent 100% processor usage

consoleShouldClose :: Console -> IO Bool
consoleShouldClose = GLFW.windowShouldClose

withConsole :: GLint -> GLint -> String -> (Console -> IO ()) -> IO ()
withConsole width height title action =
    withWindow (fromIntegral windowWidth) (fromIntegral windowHeight) title $ \win -> do
        GLFW.setWindowSizeCallback win (Just resizeCallback)
        GL.viewport $= (GL.Position 0 0, GL.Size windowWidth windowHeight)
        GL.matrixMode $= GL.Projection
        GL.loadIdentity
        GL.ortho2D 0.0 (fromIntegral windowWidth) (fromIntegral windowHeight) 0.0
        GL.matrixMode $= GL.Modelview 0

        tex <- loadTexture "terminal8x8.png"
        GL.texture GL.Texture2D $= GL.Enabled
        GL.activeTexture $= GL.TextureUnit 0
        GL.textureBinding GL.Texture2D $= Just tex

        action win
    where
        windowWidth :: GLint
        windowWidth = width * truncate charWidth

        windowHeight :: GLint
        windowHeight = height * truncate charHeight

        resizeCallback :: GLFW.Window -> Int -> Int -> IO ()
        resizeCallback _ w h = do
            putStrLn $ "resize" ++ show (w, h)
            GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))


-- | Creates a window using GLFW and executes an action
withWindow :: Int     -- ^ Width
           -> Int     -- ^ Height
           -> String  -- ^ Window caption
           -> (GLFW.Window -> IO ())
           -> IO ()
withWindow width height title action = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    success <- GLFW.init
    when success $ do
        GLFW.windowHint $ GLFW.WindowHint'RefreshRate 60
        GLFW.windowHint $ GLFW.WindowHint'Resizable True
        maybeWindow <- GLFW.createWindow width height title Nothing Nothing
        case maybeWindow of
            Just window -> do
                GLFW.makeContextCurrent $ Just window
                GLFW.setCursorInputMode window GLFW.CursorInputMode'Hidden
                GLFW.swapInterval 1
                finally (action window) $ do
                    GLFW.destroyWindow window
                    GLFW.terminate
            Nothing     -> return ()
    where
        simpleErrorCallback e s = putStrLn $ unwords [show e, show s]