module Main where

import Data.Char
import ConsoleGLUT


display :: IO ()
display = do
    clearConsole

    mapM_ (\c -> drawChar (ord '@') (c, 5)) [1..30]

    drawString "Hello World!" (2, 2)
    drawString "Testing" (5, 3)

    flushConsole Console

--mainloop con . not =<< consoleShouldClose con


main :: IO ()
main = runConsole 80 50 "Necromancer Simulator 2014" $ display

