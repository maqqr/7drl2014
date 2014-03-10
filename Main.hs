module Main where

import Data.Char
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Graphics.UI.GLFW (Key(..))

import GameData
import Console
import Color


worldmapTileToChar :: WorldMapTile -> (Int, Color, Color)
worldmapTileToChar Plains   = (ord '"', (0.4, 0.7, 0.4), (0.9, 0.9, 0.1))
worldmapTileToChar Mountain = (ord '^', (0.9, 0.9, 0.9), (0.1, 0.1, 0.1))
worldmapTileToChar Forest   = (5      , (0.1, 0.7, 0.1), (0.1, 0.1, 0.1))
worldmapTileToChar Lake     = (ord '=', (0.0, 0.0, 0.3), (0.3, 0.3, 0.9))
worldmapTileToChar River    = (ord '~', (0.3, 0.3, 0.5), (0.6, 0.6, 1.0))
worldmapTileToChar _        = (ord '?', (0.5, 0.5, 0.5), (0.5, 0.5, 0.5))

type GameState a = StateT Game IO a

type ConsoleLoop = Bool -> Console -> GameState ()

consoleLoop :: Console -> ConsoleLoop -> GameState ()
consoleLoop con f = lift (consoleIsRunning con) >>= \run -> lift (flushConsole con) >>= f run

townmap :: ConsoleLoop
townmap False _  = return ()
townmap True con = do
    lift $ do
        clearConsole
        drawString whiteChar "Townmap" (0, 0)

    consoleLoop con townmap


worldmap :: ConsoleLoop
worldmap False _  = return ()
worldmap True con = do
    wmap <- fmap worldTileMap get
    lift $ do
        clearConsole
        sequence_ $ M.foldrWithKey (\xy tile iolist -> drawTile xy tile:iolist) [] wmap

    consoleLoop con worldmap
    where
        drawTile :: Point -> WorldMapTile -> IO ()
        drawTile xy t = let (ascii, col, col2) = worldmapTileToChar t
                        in colorChar2 col col2 ascii xy


characterCreation :: ConsoleLoop
characterCreation = characterCreation' ""
    where
        characterCreation' :: String -> ConsoleLoop
        characterCreation' _    False _  = return ()
        characterCreation' name True con = do
            lift $ do
                clearConsole
                drawString whiteChar ("Enter your name: " ++ name) (3, 5)

            case pressedKeys con of
                (k:_) -> handleInput k
                _     -> consoleLoop con (characterCreation' name)

            where
                handleInput k
                    | k == Key'Enter     = consoleLoop (advanceInput con) worldmap
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
    titleString "Necromancer Simulator 2014" (10, 3)
    titleString "(S)tart New Game" (5, 8)
    titleString "(Q)uit Game" (5, 10)
    when (con `keyPressed` Key'S) $ newGame >>= runGame (advanceInput con)
    unless (con `keyPressed` Key'Q) $ consoleIsRunning con >>= \run -> flushConsole con >>= mainmenu run
    where
        titleString = drawString (colorChar2 (light red) (0.4, 0.3, 0.3))

runGame :: Console -> Game -> IO ()
runGame = evalStateT . characterCreation True

main :: IO ()
main = withConsole 80 60 "Necromancer Simulator 2014" (mainmenu True)
