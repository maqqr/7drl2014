module Main where

import Data.Char
import Console


mainloop :: Console -> Bool -> IO ()
mainloop _ False = return ()
mainloop con True = do
    clearConsole

    mapM_ (\c -> drawChar (ord '@') (c, 5)) [1..30]

    drawString "Hello World!" (2, 2)
    drawString "Testing" (5, 3)

    flushConsole con
    mainloop con . not =<< consoleShouldClose con


main :: IO ()
main = withConsole 80 60 "Necromancer Simulator 2014" $ \con -> mainloop con True
