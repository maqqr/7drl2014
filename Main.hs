module Main where

import Data.Char
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Graphics.UI.GLFW (Key(..))

import GameData
import Console
import Color

type GameState a = StateT Game IO a

type ConsoleLoop = Bool -> Console -> GameState ()

type CharInfo = (Int, Color, Color)

consoleLoop :: Console -> ConsoleLoop -> GameState ()
consoleLoop con f = lift (consoleIsRunning con) >>= \run -> lift (flushConsole con) >>= f run

townmap :: ConsoleLoop
townmap False _  = return ()
townmap True con = do
    gstate <- get
    lift $ do
        clearConsole
        sequence_ $ M.foldrWithKey (\xy tile iolist -> drawTile xy tile:iolist) [] (tileMap gstate)

    consoleLoop con townmap
    where
        tileToChar :: Tile -> CharInfo
        tileToChar Floor     = (ord '.', (0.5, 0.5, 0.5), (0.5, 0.5, 0.5))
        tileToChar Road      = (ord '.', (0.5, 0.3, 0.1), (0.5, 0.3, 0.1))
        tileToChar WallWood  = (ord '#', (0.5, 0.3, 0.1), (0.5, 0.3, 0.1))
        tileToChar WallStone = (ord '#', (0.7, 0.7, 0.7), (0.7, 0.7, 0.7))
        tileToChar Tree      = (5      , (0.1, 0.7, 0.1), (0.1, 0.1, 0.1))
        tileToChar Water     = (ord '=', (0.0, 0.0, 0.3), (0.3, 0.3, 0.9))
        tileToChar DoorClose = (ord '+', (0.6, 0.3, 0.1), (0.6, 0.3, 0.1))
        tileToChar DoorOpen  = (ord '/', (0.6, 0.3, 0.1), (0.6, 0.3, 0.1))
        tileToChar Gate      = (ord '0', (0.4, 0.2, 0.1), (0.4, 0.2, 0.1))

        drawTile :: Point -> Tile -> IO ()
        drawTile xy t = let (ascii, col, col2) = tileToChar t
                        in colorChar2 col col2 ascii xy


worldmap :: ConsoleLoop
worldmap False _  = return ()
worldmap True con = do
    gstate <- get
    lift $ do
        clearConsole

        -- Draw map and villages
        sequence_ $ M.foldrWithKey (\xy tile iolist -> drawTile xy (worldmapTileToChar tile):iolist) [] (worldTileMap gstate)
        sequence_ $ M.foldrWithKey (\xy tile iolist -> drawTile xy (villageToChar tile):iolist) [] (worldVillageMap gstate)

        -- Draw player
        colorChar (0.8, 0.3, 0.5) (ord '@') (worldmapPosition gstate)

    when (con `keyPressed` Key'A) $ movePlayer (-1,  0)
    when (con `keyPressed` Key'D) $ movePlayer ( 1,  0)
    when (con `keyPressed` Key'W) $ movePlayer ( 0, -1)
    when (con `keyPressed` Key'S) $ movePlayer ( 0,  1)

    consoleLoop con worldmap
    where
        villageToChar :: Village -> CharInfo
        villageToChar (Village _ _ False) = (ord 'o', (0.7, 0.4, 0.2), (0.3, 0.1, 0.0))
        villageToChar (Village _ _ True)  = (ord 'o', (0.9, 0.1, 0.1), (0.2, 0.1, 0.0))
        villageToChar Castle              = (ord '#', (0.9, 0.9, 0.9), (0.1, 0.1, 0.1))

        worldmapTileToChar :: WorldMapTile -> CharInfo
        worldmapTileToChar Plains    = (ord '.', (0.4, 0.7, 0.4), (0.9, 0.9, 0.1))
        worldmapTileToChar Mountain  = (ord '^', (0.9, 0.9, 0.9), (0.1, 0.1, 0.1))
        worldmapTileToChar Forest    = (5      , (0.1, 0.7, 0.1), (0.1, 0.1, 0.1))
        worldmapTileToChar Lake      = (ord '=', (0.0, 0.0, 0.3), (0.3, 0.3, 0.9))
        worldmapTileToChar River     = (ord '~', (0.3, 0.3, 0.5), (0.6, 0.6, 1.0))
        worldmapTileToChar TowerTile = (ord 'I', (0.8, 0.3, 0.6), (0.4, 0.1, 0.3))
        worldmapTileToChar _         = (ord '?', (0.5, 0.5, 0.5), (0.5, 0.5, 0.5))

        drawTile :: Point -> CharInfo -> IO ()
        drawTile xy t = let (ascii, col, col2) = t
                        in colorChar2 col col2 ascii xy

        blocked :: Game -> Point -> Bool
        blocked game xy = fromMaybe True . fmap solidTile $ M.lookup xy (worldTileMap game)
            where
                solidTile Mountain = True
                solidTile Lake     = True
                solidTile _        = False

        movePlayer :: Point -> GameState ()
        movePlayer delta = get >>= movePlayer'
            where
                movePlayer' gstate = when (not $ blocked gstate newPos) $ modify (\g -> g { worldmapPosition = newPos })
                    where
                        oldxy  = worldmapPosition gstate
                        newPos = oldxy ^+^ delta


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
