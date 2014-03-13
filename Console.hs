module Console where

import Data.IORef
import Data.Char
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad
import Control.Exception (finally)
import Control.Concurrent (threadDelay)
import Control.Applicative
import Graphics.Rendering.OpenGL (($=), GLfloat, GLint)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.GLUtil as GLU

import Color

charWidth :: GLfloat
charWidth = 8.0

charHeight :: GLfloat
charHeight = 8.0


data Console = Console {
    consoleWindow :: GLFW.Window,
    inputRef      :: IORef (Set GLFW.Key),
    input         :: Set GLFW.Key,
    oldInput      :: Set GLFW.Key
}

-- | Loads texture and sets filtering and wrapping modes
loadTexture :: FilePath -> IO GL.TextureObject
loadTexture texturePath = do
    tex <- either error id <$> GLU.readTexture texturePath
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Nearest)
    GLU.texture2DWrap $= (GL.Mirrored, GL.ClampToEdge)
    return tex


type CharacterRenderer = Int -> (Int, Int) -> IO ()

whiteChar :: CharacterRenderer
whiteChar = colorChar (1.0, 1.0, 1.0)

colorChar :: Color -> CharacterRenderer
colorChar col = colorChar2 col col

-- | Draws a single character with two colors
colorChar2 :: Color       -- ^ Top color
           -> Color       -- ^ Bottom color
           -> Int         -- ^ ASCII code of the character
           -> (Int, Int)  -- ^ Position on the console
           -> IO ()
colorChar2 (r,g,b) (r2,g2,b2) charCode (x', y') = GL.renderPrimitive GL.Quads . vertexInfo $ quad
    where
        vertexInfo :: [(GLfloat,GLfloat,GLfloat,GLfloat,GLfloat,GLfloat,GLfloat)] -> IO ()
        vertexInfo = mapM_ (\(xx,yy,u,v,r',g',b') -> do
            GL.color (GL.Color3 r' g' b')
            GL.texCoord (GL.TexCoord2 u v)
            GL.vertex (GL.Vertex3 xx yy 0.0))

        cw = charWidth
        ch = charHeight
        (x, y) = (fromIntegral x' * charWidth, fromIntegral y' * charHeight)
        texmapW = charWidth * 16.0
        texmapH = charHeight * 16.0
        texX = fromIntegral (charCode `mod` 16) * charWidth
        texY = fromIntegral (charCode `quot` 16) * charHeight

        quad :: [(GLfloat,GLfloat,GLfloat,GLfloat,GLfloat,GLfloat,GLfloat)]
        quad = [(x,    y,    texX / texmapW,      texY / texmapH,      r,  g,  b),
                (x+cw, y,    (texX+cw) / texmapW, texY / texmapH,      r,  g,  b),
                (x+cw, y+ch, (texX+cw) / texmapW, (texY+ch) / texmapH, r2, g2, b2),
                (x,    y+ch, texX / texmapW,      (texY+ch) / texmapH, r2, g2, b2)]


-- | Draws a string on the console
drawString :: CharacterRenderer -- ^ Character drawing function
           -> String            -- ^ String to draw
           -> (Int, Int)        -- ^ Position on the console
           -> IO ()
drawString _ [] _ = return ()
drawString cr (c:hars) (x, y) = cr (ord c) (x, y) >> drawString cr hars (x+1, y)

drawStringCentered :: CharacterRenderer -> String -> (Int, Int) -> IO ()
drawStringCentered cr str (x, y) = drawString cr str (x - offset, y)
    where
        offset = length str `quot` 2

drawFrame :: CharacterRenderer -> (Int, Int) -> Int -> Int -> IO ()
drawFrame cr (sx, sy) width' height' = mapM_ (uncurry cr) $ concat [top, bottom, left, right, corners]
    where
        (width, height) = (width' - 1, height' - 1)
        top     = [(ord '=', (x, sy))        | x <- [sx..sx+width]]
        bottom  = [(ord '=', (x, sy+height)) | x <- [sx..sx+width]]
        left    = [(ord 'I', (sx,       y))  | y <- [sy..sy+height]]
        right   = [(ord 'I', (sx+width, y))  | y <- [sy..sy+height]]
        corners = [(ord '/', (sx, sy)), (ord '\\', (sx+width, sy)),
                   (ord '\\', (sx,sy+height)), (ord '/', (sx+width,sy+height))]


-- | Clears the console
clearConsole :: IO ()
clearConsole = GL.clear [GL.ColorBuffer, GL.DepthBuffer] >> GL.loadIdentity

-- | Flushes the console updates to the screen and updates keyboard input
flushConsole :: Console -> IO Console
flushConsole con = do
    GLFW.swapBuffers (consoleWindow con)
    GL.flush
    GLFW.pollEvents
    newKeys <- readIORef (inputRef con)
    threadDelay 20000 -- small delay to prevent 100% processor usage
    return con { input = newKeys, oldInput = input con }

-- | Returns false if the user closed the window
consoleIsRunning :: Console -> IO Bool
consoleIsRunning = fmap not . GLFW.windowShouldClose . consoleWindow

keyboardCallback :: IORef (Set GLFW.Key) -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyboardCallback keyref _ key _ GLFW.KeyState'Pressed _ = do
    keys <- readIORef keyref
    writeIORef keyref $ S.insert key keys

keyboardCallback keyref _ key _ GLFW.KeyState'Released _ = do
    keys <- readIORef keyref
    writeIORef keyref $ S.delete key keys

keyboardCallback _ _ _ _ _ _ = return ()

keyPressed :: Console -> GLFW.Key -> Bool
keyPressed (Console _ _ keys oldkeys) key = key `S.member` keys && not (key `S.member` oldkeys)

keyReleased :: Console -> GLFW.Key -> Bool
keyReleased (Console _ _ keys oldkeys) key = not (key `S.member` keys) && key `S.member` oldkeys

keyDown :: Console -> GLFW.Key -> Bool
keyDown (Console _ _ keys _) key = key `S.member` keys

keysPressed :: Console -> [GLFW.Key] -> Bool
keysPressed con = any (con `keyPressed`)

anyKeysDown :: Console -> [GLFW.Key] -> Bool
anyKeysDown con = any (con `keyDown`)

allKeysDown :: Console -> [GLFW.Key] -> Bool
allKeysDown con = all (con `keyDown`)

advanceInput :: Console -> Console
advanceInput (Console win ref keys _) = Console win ref keys keys

-- | Creates a "console" window and executes an action
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

        keyRef <- newIORef S.empty
        GLFW.setKeyCallback win $ Just (keyboardCallback keyRef)
        action (Console win keyRef S.empty S.empty)
    where
        windowWidth :: GLint
        windowWidth = width * truncate charWidth

        windowHeight :: GLint
        windowHeight = height * truncate charHeight

        resizeCallback :: GLFW.Window -> Int -> Int -> IO ()
        resizeCallback _ w h = GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))


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
                --GLFW.setCursorInputMode window GLFW.CursorInputMode'Hidden
                GLFW.swapInterval 1
                finally (action window) $ do
                    GLFW.destroyWindow window
                    GLFW.terminate
            Nothing -> return ()
    where
        simpleErrorCallback e s = putStrLn $ unwords [show e, show s]
