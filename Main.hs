module Main where

import Data.Char
import Control.Monad
import Control.Monad.State
import Graphics.UI.GLFW (Key(..))

import GameData
import Console

type GameState a = StateT Game IO a

mainloop :: Bool -> Console -> GameState ()
mainloop False _  = return ()
mainloop True con = do
    lift $ do
        clearConsole

        mapM_ (\c -> drawChar (ord '@') (c, 5)) [1..30]

        drawString "Hello World!" (2, 2)
        drawString "Testing" (5, 3)

    lift (consoleIsRunning con) >>= \run -> lift (flushConsole con) >>= mainloop run


mainmenu :: Bool -> Console -> IO ()
mainmenu False _  = return ()
mainmenu True con = do
    clearConsole
    drawString "Necromancer Simulator 2014" (10, 3)
    case con `keyPressed` Key'A of
        True  -> newGame >>= runGame con
        False -> consoleIsRunning con >>= \run -> flushConsole con >>= mainmenu run

runGame :: Console -> Game -> IO ()
runGame = evalStateT . mainloop True

main :: IO ()
main = withConsole 80 60 "Necromancer Simulator 2014" (mainmenu True)
