module Color where

import Graphics.Rendering.OpenGL (GLfloat)

type Color = (GLfloat, GLfloat, GLfloat)

makeColor :: (Double, Double, Double) -> Color
makeColor (r, g, b) = (rf r, rf g, rf b)
    where
        rf = realToFrac

white :: Color
white = makeColor (1.0, 1.0, 1.0)

red :: Color
red = makeColor (1.0, 0.1, 0.1)

green :: Color
green = makeColor (0.1, 1.0, 0.1)

blue :: Color
blue = makeColor (0.1, 0.1, 1.0)

dark :: Color -> Color
dark (r, g, b) = (r * c, g * c, b * c)
    where
        c = 0.5

light :: Color -> Color
light (r, g, b) = clampColor (r * c + 0.1, g * c + 0.1, b * c + 0.1)
    where
        c = 1.5

clampColor :: Color -> Color
clampColor (r, g, b) = (clamp r, clamp g, clamp b)
    where
        min' = 0.0
        max' = 1.0
        clamp x
            | x > max'  = max'
            | x < min'  = min'
            | otherwise = x
