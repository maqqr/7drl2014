module Main where

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Graphics.UI.GLFW (Key(..))
import System.Random (randomRIO)

import TownGenerator
import GameData
import Console
import Color
import Line

type GameState a = StateT Game IO a

type ConsoleLoop = Bool -> Console -> GameState ()

type CharInfo = (Int, Color, Color)

moveKeys :: [([Key], Point)]
moveKeys = [([Key'H, Key'Pad4], (-1, 0)), ([Key'L, Key'Pad6], (1, 0)),
            ([Key'K, Key'Pad8], (0, -1)), ([Key'J, Key'Pad2], (0, 1)),
            ([Key'Y, Key'Pad7], (-1,-1)), ([Key'U, Key'Pad9], (1,-1)),
            ([Key'B, Key'Pad1], (-1, 1)), ([Key'N, Key'Pad3], (1, 1)),
            ([Key'Pad5], (0, 0))]

consoleLoop :: Console -> ConsoleLoop -> GameState ()
consoleLoop con f = lift (consoleIsRunning con) >>= \run -> lift (flushConsole con) >>= f run

townmap :: ConsoleLoop
townmap False _  = return ()
townmap True con = do
    gstate <- get
    lift $ do
        clearConsole
        let tmap = tileMap gstate
        let playerPlace = place . player $ gstate
        sequence_ $ foldr (\xy -> mapDrawer gstate xy $ M.lookup xy tmap) [] (tilesInSight playerPlace)

        -- Draw corpses, npcs & zombies
        sequence_ $ M.foldrWithKey (\xy c iolist -> drawCorpse (xy, c):iolist) [] (corpseMap gstate)
        sequence_ $ M.foldrWithKey (\xy n iolist -> drawNpc (xy, n):iolist) [] (M.filterWithKey (\k _ -> visionFilter gstate playerPlace k) $ npcMap gstate)
        sequence_ $ M.foldrWithKey (\xy z iolist -> drawZombie (xy, z):iolist) [] (minionMap gstate)

        colorChar (0.8, 0.3, 0.5) (ord '@') playerPlace

        -- Draw message buffer
        drawFrame whiteChar (0, 50) 80 10
        drawMessageBuffer whiteChar (messageBuffer gstate) (2, 51)

        -- Draw player health
        let hp' = hp . player $ gstate
        let maxHp' = maxHp . player $ gstate
        drawStringCentered whiteChar ("Hit points: " ++ show hp' ++ " / " ++ show maxHp') (40, 50)

    -- Move player
    mapM_ (\(ks, delta) -> when (con `keysPressed` ks) (movePlayer delta)) moveKeys

    consoleLoop con townmap
    where
        sightR = 14
        tilesInSight (px, py) = [(px+x, py+y) | x <- [-sightR..sightR], y <- [-sightR..sightR], x*x + y*y < sightR * sightR]

        visionFilter :: Game -> Point -> Point -> Bool
        visionFilter gstate (px, py) (x, y) =
            abs (px - x) <= sightR && abs (py - y) <= sightR && lineOfSight (notTransparent gstate) (px, py) (x, y)

        drawMessageBuffer :: CharacterRenderer -> [String] -> Point -> IO ()
        drawMessageBuffer _  []     _      = return ()
        drawMessageBuffer cr (m:ms) (x, y) = drawString cr m (x, y) >> drawMessageBuffer cr ms (x, y+1)

        mapDrawer :: Game -> Point -> Maybe Tile -> [IO ()] -> [IO ()]
        mapDrawer _ _ Nothing = id
        mapDrawer gstate xy (Just t)
            | lineOfSight (notTransparent gstate) (place . player $ gstate) xy = (:) (drawTile xy t)
            | otherwise = id

        tileToChar :: Tile -> CharInfo
        tileToChar Floor     = (ord '.', (0.5, 0.5, 0.5), (0.5, 0.5, 0.5))
        tileToChar Grass     = (ord '.', (0.2, 0.8, 0.2), (0.2, 0.8, 0.2))
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

        drawNpc :: (Point, Npc) -> IO ()
        drawNpc (xy, n) = let (ascii, col1, col2) = npcData n
                          in colorChar2 col1 col2 ascii xy 
            where
                npcData :: Npc -> CharInfo
                npcData Guard = (ord '@', (0.7, 0.7, 0.7), (0.2, 0.2, 0.7))
                npcData Child = (ord '@', (0.7, 0.7, 0.7), (0.1, 0.1, 0.9))
                npcData King  = (ord '@', (0.7, 0.7, 0.5), (0.7, 0.7, 0.5))
                npcData _     = (ord '@', (0.6, 0.6, 0.6), (0.8, 0.8, 0.8))

        -- riittäisikö tässä vain päällimmäisen zombin piirto?
        drawZombie :: (Point, [Zombi]) -> IO ()
        drawZombie (_,  []) = return ()
        drawZombie (xy, (z:zs)) = do
                colorChar2 col1 col2 ascii xy
                drawZombie (xy, zs)
            where
                (ascii, col1, col2) = zombiData z

                zombiData :: Zombi -> CharInfo
                zombiData GuardZombi = (ord 'Z', (0.7, 0.2, 0.2), (0.7, 0.7, 0.7))
                zombiData EliteZombi = (ord 'Z', (0.7, 0.2, 0.2), (0.7, 0.7, 0.7))
                zombiData KingZombi  = (ord 'Z', (0.7, 0.7, 0.4), (0.8, 0.8, 0.5))
                zombiData _          = (ord 'Z', (0.7, 0.1, 0.1), (1.0, 0.1, 0.1))

        drawCorpse :: (Point, [Corpse]) -> IO ()
        drawCorpse (_,  []) = return ()
        drawCorpse (xy, (x:_)) = colorChar2 (0.5, 0.5, 0.5) (0.5, 0.5, 0.5) (ord '&') xy

        blocked :: Game -> Point -> Bool
        blocked game xy = fromMaybe True . fmap tileBlocks $ M.lookup xy (tileMap game)

        notTransparent :: Game -> Point -> Bool
        notTransparent game xy = fromMaybe True . fmap tileNotTransparent $ M.lookup xy (tileMap game)

        movePlayer :: Point -> GameState ()
        movePlayer delta = get >>= movePlayer'
            where
                movePlayer' gstate = when (not $ blocked gstate newPos) $ do
                        modify (\g -> g { player = oldplayer { place = newPos } })
                        updateMap updateNpc npcMap
                        updateMap updateZombie minionMap
                    where
                        oldplayer = player gstate
                        oldxy     = place oldplayer
                        newPos    = oldxy ^+^ delta

        --updateMap :: GameState ()
        updateMap f ml = do
            gstate <- get
            g <- foldM f gstate (M.keys $ ml gstate)
            put g

        updateNpc :: Game -> Point -> GameState Game
        updateNpc gstate xy = do
            rx <- lift $ (randomRIO (-1, 1) :: IO Int)
            ry <- lift $ (randomRIO (-1, 1) :: IO Int)
            let targetNpc = M.lookup xy (npcMap gstate)
            case targetNpc of
                Just npc -> return $ moveNpc npc xy (xy ^+^ (rx, ry)) gstate
                Nothing  -> return gstate
        
        moveNpc :: Npc -> Point -> Point -> Game -> Game
        moveNpc npc start end gstate
            | isNothing (M.lookup end $ npcMap gstate) = gstate { npcMap = M.insert end npc . M.delete start $ npcMap gstate }
            | otherwise = gstate

        updateZombie :: Game -> Point -> GameState Game
        updateZombie gstate xy = case M.lookup xy (minionMap gstate) of
            Just zombieList -> return gstate
            Nothing         -> return gstate

        moveZombi :: Zombi -> Point -> Point -> Game -> Game
        moveZombi npc start end gstate = gstate


worldmap :: ConsoleLoop
worldmap False _  = return ()
worldmap True con = do
    gstate <- get
    lift $ do
        clearConsole
        print $ worldmapPosition gstate

        -- Draw map and villages
        sequence_ $ M.foldrWithKey (\xy tile iolist -> drawTile xy (worldmapTileToChar tile):iolist) [] (worldTileMap gstate)
        sequence_ $ M.foldrWithKey (\xy tile iolist -> drawTile xy (villageToChar tile):iolist) [] (worldVillageMap gstate)

        -- Draw player
        colorChar (0.8, 0.3, 0.5) (ord '@') (worldmapPosition gstate)

        -- Draw tower info
        drawFrame whiteChar (0, 40) 80 20
        drawStringCentered whiteChar "INFORMATION ABOUT YOUR NECROMANCER TOWER" (40, 40)
        drawInfo (5, 44) gstate (zombieStr.tower) "Zombie strength: "
        drawInfo (5, 46) gstate (zombieCon.tower) "Zombie ???: "
        drawInfo (5, 48) gstate (zombieMax.tower) "Max zombie count: "

        drawInfo (40, 44) gstate (playerHp.tower) "Player max HP: "
        drawInfo (40, 46) gstate (spellDmg.tower) "Spell damage: "
        drawInfo (40, 48) gstate (playerHp.tower) "Spells up (?): "

    -- Move player
    mapM_ (\(ks, delta) -> when (con `keysPressed` ks) (movePlayer delta)) moveKeys

    -- todo: upgrade tower if key was pressed

    -- Enter village
    when (con `keysPressed` [Key'Enter]) $ enterVillage

    consoleLoop con worldmap
    where
        drawInfo :: Show a => Point -> Game -> (Game -> a) -> String -> IO ()
        drawInfo xy gstate g str = drawString whiteChar (str ++ show (g gstate)) xy

        enterVillage :: GameState ()
        enterVillage = do
            gstate <- get
            let targetVillage = M.lookup (worldmapPosition gstate) (worldVillageMap gstate)
            case targetVillage of
                Just village -> do
                    let oldplayer = player gstate
                    let playerPosOnTown = (0, 0) -- todo: make random?
                    (rndVillage, npcs) <- lift $ randomVillageMap (0, 0, 80, 50) (villageSize village)
                    modify (\g -> g { tileMap = rndVillage,
                                      npcMap = npcs,
                                      minionMap = M.fromList [(playerPosOnTown, zombieArmy gstate)],
                                      player = oldplayer { place = playerPosOnTown },
                                      messageBuffer = ["You entered " ++ villageName village ++ "."] })
                    consoleLoop con townmap
                Nothing -> return ()

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
                drawString whiteChar ("Enter your name:") (30, 5)
                drawString (colorChar2 (light red) (0.4, 0.3, 0.3)) name (30, 7)

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
    drawFrame (colorChar (1.0, 0, 0)) (23, 6) 30 3
    titleString " Necromancer Simulator 2014 " (24, 7)
    titleString "(S)tart New Game" (30, 14)
    titleString "(Q)uit Game" (30, 16)
    when (con `keyPressed` Key'S) $ newGame >>= runGame (advanceInput con)
    unless (con `keyPressed` Key'Q) $ consoleIsRunning con >>= \run -> flushConsole con >>= mainmenu run
    where
        titleString = drawString (colorChar2 (light red) (0.4, 0.3, 0.3))

runGame :: Console -> Game -> IO ()
runGame = evalStateT . characterCreation True

main :: IO ()
main = withConsole 80 60 "Necromancer Simulator 2014" (mainmenu True)
