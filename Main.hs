module Main where

import Data.Char
import qualified Data.Set as S
import Control.Monad
import Control.Monad.State
import Graphics.UI.GLFW (Key(..))

import GameData
import Console

type GameState a = StateT Game IO a

type ConsoleLoop = Bool -> Console -> GameState ()

consoleLoop :: Console -> ConsoleLoop -> GameState ()
consoleLoop con f = lift (consoleIsRunning con) >>= \run -> lift (flushConsole con) >>= f run

townmap :: ConsoleLoop
townmap False _  = return ()
townmap True con = do
    lift $ do
        clearConsole
        drawString "Townmap" (0, 0)

    consoleLoop con townmap

worldmap :: ConsoleLoop
worldmap False _  = return ()
worldmap True con = do
    lift $ do
        clearConsole
        drawString "(draw map here)" (35, 24)

    consoleLoop con worldmap



characterCreation :: ConsoleLoop
characterCreation = characterCreation' ""
    where
        characterCreation' :: String -> ConsoleLoop
        characterCreation' _    False _  = return ()
        characterCreation' name True con = do
            lift $ do
                clearConsole
                drawString ("Enter your name: " ++ name) (3, 5)

            case pressedKeys con of
                (k:_) -> handleInput k
                _     -> consoleLoop con (characterCreation' name)

            where
                handleInput k
                    | k == Key'Enter     = consoleLoop con worldmap
                    | k == Key'Backspace = consoleLoop con (characterCreation' $ eraseLastElem name)
                    | k `elem` Key'A `enumFromTo` Key'Z = consoleLoop con (characterCreation' $ name ++ [keyToChar k])
                    | otherwise          = consoleLoop con (characterCreation' name)

        eraseLastElem :: [a] -> [a]
        eraseLastElem []     = []
        eraseLastElem (_:[]) = []
        eraseLastElem (x:xs) = x : eraseLastElem xs

        pressedKeys :: Console -> [Key]
        pressedKeys con = filter (\k -> con `keyPressed` k) (S.elems $ input con)

        keyToChar :: Key -> Char
        keyToChar key = toLower . last . show $ key


mainmenu :: Bool -> Console -> IO ()
mainmenu False _  = return ()
mainmenu True con = do
    clearConsole
    drawString "Necromancer Simulator 2014" (10, 3)
    drawString "(S)tart New Game" (5, 8)
    drawString "(Q)uit Game" (5, 10)
    when (con `keyPressed` Key'S) $ newGame "" >>= runGame con
    unless (con `keyPressed` Key'Q) $ consoleIsRunning con >>= \run -> flushConsole con >>= mainmenu run

runGame :: Console -> Game -> IO ()
runGame = evalStateT . characterCreation True

main :: IO ()
main = withConsole 80 60 "Necromancer Simulator 2014" (mainmenu True)
