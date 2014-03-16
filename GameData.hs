{-# LANGUAGE RecordWildCards #-}
module GameData where

import qualified Data.Map as M
import Data.Map ((\\),Map)
import System.Random
import Data.Maybe
import Debug.Trace

ptrace :: Show a => a -> a
ptrace a = trace (show a) a

ptrace' :: Show a => String -> a -> a
ptrace' s a = trace ("\n" ++ s ++ ";" ++ show a) a

type Point = (Int, Int)

data Tile = Floor
          | Grass
          | Road
          | WallWood
          | WallStone
          | Tree
          | Water
          | DoorClose | DoorOpen
          | Gate
          deriving (Eq, Show)

tileBlocks :: Tile -> Bool
tileBlocks WallWood  = True
tileBlocks WallStone = True
tileBlocks Tree      = True
tileBlocks Water     = True
tileBlocks Gate      = True
tileBlocks _         = False

tileNotTransparent :: Tile -> Bool
tileNotTransparent WallWood  = True
tileNotTransparent WallStone = True
tileNotTransparent Tree      = True
tileNotTransparent Water     = True
tileNotTransparent Gate      = True
tileNotTransparent DoorClose = True
tileNotTransparent _         = False

sign :: Int -> Int
sign x
    | x < 0 = -1
    | x > 0 =  1
    | otherwise = 0

(^+^) :: Point -> Point -> Point
(x, y) ^+^ (x', y') = (x + x', y + y')

(^-^) :: Point -> Point -> Point
(x, y) ^-^ (x', y') = (x - x', y - y')

normalize :: Point -> Point
normalize (x, y) = (sign x, sign y)

type TileMap = Map Point Tile

data Village = Castle | Village {
    villageName     :: String,
    villageSize     :: Int,
    playerStatus    :: Bool
}   deriving (Eq, Show)

data WorldMapTile = Forest 
                  | Plains
                  | Mountain
                  | Lake | River
                  | WorldRoad
                  | TowerTile
                  deriving (Eq, Show)

type WorldTileMap = Map Point WorldMapTile

type WorldVillageMap = Map Point Village

data Spell = RiseUndead 
           | ForceBolt
           | Fireball
           | DrainLife
           deriving (Eq, Show)

data Player = Player {
    playerName    :: String,
    place         :: Point,
    points        :: Int,
    hp            :: Int,
    maxHp         :: Int,
    spells        :: [Spell]
}   deriving (Eq, Show)

data Tower = Tower {
    zombieStr   :: Int,
    zombieCon   :: Int,
    zombieMax   :: Int,

    playerHp    :: Int,
    spellDmg    :: Int,
    spellsUp    :: Int
}   deriving (Eq, Show)

data Zombi  = Zombi
            | GuardZombi 
            | EliteZombi 
            | KingZombi
            deriving (Eq, Show)

type MinionMap = Map Point [Zombi]

data Corpse = Corpse {
    npcType     :: Npc
}   deriving (Eq, Show)

type CorpseMap = Map Point [Corpse]

data Npc = Guard
         | King
         | MaleUnarmed | FemaleUnarmed | Male | Female
         | Child
         deriving (Eq, Bounded, Enum)

instance Random Npc where
    random g = case randomR (fromEnum (minBound :: Npc), fromEnum (maxBound :: Npc)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

instance Show Npc where
    show MaleUnarmed   = "unarmed male"
    show FemaleUnarmed = "unarmed female"
    show Male          = "male"
    show Female        = "female"
    show Guard         = "guard"
    show Child         = "child"
    show King          = "King"

type NpcMap = Map Point Npc

data Game = Game {
    player           :: Player,
    tower            :: Tower,
    zombieArmy       :: [Zombi],
    worldTileMap     :: WorldTileMap,
    worldVillageMap  :: WorldVillageMap,
    worldmapPosition :: Point,

    minionMap        :: MinionMap,
    npcMap           :: NpcMap,
    tileMap          :: TileMap,
    corpseMap        :: CorpseMap,

    messageBuffer    :: [String]
}   deriving (Eq, Show)


--Funktiot--


addMsg :: String -> Game -> Game
addMsg str game
    | length buf >= maxBufferSize = game { messageBuffer = str : take (maxBufferSize-1) buf }
    | otherwise                   = game { messageBuffer = str : buf }
    where
        buf = messageBuffer game
        maxBufferSize = 8

aoe :: Point -> Int -> [Point]
aoe (x',y') r = [(x'+x, y'+y) | x <- [-r..r], y <- [-r..r], x^2+y^2<=r^2] 

convertNpcToCorpse :: Maybe Npc -> Maybe [Corpse] -> Maybe [Corpse]
convertNpcToCorpse Nothing Nothing = Nothing
convertNpcToCorpse Nothing oldies = oldies
convertNpcToCorpse (Just npc) Nothing = Just [Corpse npc]
convertNpcToCorpse (Just npc) (Just oldies) = Just (Corpse npc:oldies)

raiseUndead :: Game -> Game
raiseUndead old@Game {..} = addCastMsg $ old {
        corpseMap = corpseMap \\ nearCorpses,
        minionMap = M.union minionMap $ M.map (\corpses -> map convertToUndead corpses) nearCorpses
    }
    where
        addCastMsg :: Game -> Game
        addCastMsg game = addMsg "You cast raise undead." game

        distance :: Point -> Point -> Int
        distance (x,y) (xs,ys) = (x - xs)^2 + (y - ys)^2

        convertToUndead :: Corpse -> Zombi
        convertToUndead (Corpse Guard) = GuardZombi
        convertToUndead (Corpse King ) = KingZombi
        convertToUndead (Corpse _    ) = Zombi

        nearCorpses = M.filterWithKey (\xy _ -> distance xy (place player) <= 5^2) corpseMap


drainLife :: Game -> Point -> IO Game
drainLife old@Game {..} xy = do
    luku <- randomRIO (1::Int, 100::Int)
    if luku < 50
        then return . addCastMsg $ old {
            corpseMap = M.alter (convertNpcToCorpse target) xy corpseMap,
            npcMap    = M.delete xy npcMap,
            player    = player { hp = (hp player) + 1 }
        }
        else return $ addMsg "You failed to cast drain life." old

    where
        addCastMsg :: Game -> Game
        addCastMsg game = case target of
            Nothing    -> addMsg "There is no one to drain life from." game
            Just (npc) -> addMsg ("You drain life from the " ++ show npc ++ ".") game

        target = M.lookup xy npcMap

forceBolt :: Game -> Point -> IO Game
forceBolt old@Game {..} xy = do
    luku <- randomRIO (1::Int, 100::Int)
    if luku < (spellsUp tower)
        then return . addCastMsg $ old {
            corpseMap = M.alter (convertNpcToCorpse target) xy corpseMap,
            npcMap    = M.delete xy npcMap
        }
    else return $ addMsg "You create magical missile, but it fades into the air." old

    where
        addCastMsg :: Game -> Game
        addCastMsg game = case target of
            Nothing  -> addMsg "There is no valid target to strike." game
            Just npc -> addMsg ("The missile strikes the " ++ show npc ++ ".") game

        target = M.lookup xy npcMap

fireball :: Game -> Point -> IO Game
fireball old@Game {..} xy = do
    effectRadius <- randomRIO (1::Int, 6::Int) --Muuta, että säteeseen vaikuttaa jokin tornin attribuutti
    return . addCastMsg $ old {
        corpseMap = foldr (\xy m -> M.alter (convertNpcToCorpse (M.lookup xy npcMap)) xy m) corpseMap (aoe xy effectRadius),
        npcMap    = foldr (\xy m -> M.delete xy m) npcMap (aoe xy effectRadius)
    }

    where
        addCastMsg :: Game -> Game
        addCastMsg game = addMsg "You cast small ball of fire that explodes devouring everything inside blast radius." game

npcAttack :: Game -> Npc -> Point -> IO Game
npcAttack old@Game {..} n xy = do
    luku <- randomRIO((0 + hit n)::Int, 100::Int)
    if luku > 50 && others == []
        then return . addDeathMsg $ old {
            minionMap = M.delete xy minionMap
        }
    else if luku > 50
        then return . addDeathMsg $ old {
            minionMap = M.insert xy others minionMap
        }
    else return old

    where
        others = getOthers.M.lookup xy $ minionMap

        getOthers :: Maybe [Zombi] -> [Zombi]
        getOthers list = case list of
            Nothing -> []
            Just (x:y) -> y

        hit :: Npc -> Int
        hit King   = 40
        hit Guard  = 20
        hit Child  = 0
        hit Male   = 15
        hit Female = 15
        hit _      = 10

        addDeathMsg :: Game -> Game
        addDeathMsg game = addMsg ("The " ++ show n ++ " killed the one of your minions!") game

zombiAttack :: Game -> Zombi -> Point -> IO Game
zombiAttack old@Game {..} z xy = do
    luku <- randomRIO(((zombieStr tower) + (hit z))::Int, 100::Int)
    if luku > 50
        then return .  addDeathMsg $ old {
            corpseMap = M.alter (convertNpcToCorpse target) xy corpseMap,
            npcMap    = M.delete xy npcMap
        }
    else return old

    where
        target = M.lookup xy npcMap

        hit :: Zombi -> Int
        hit GuardZombi = 10
        hit EliteZombi = 20
        hit KingZombi = 30
        hit _ = 0

        addDeathMsg :: Game -> Game
        addDeathMsg game = addMsg ("The " ++ show z ++ " killed the " ++ show target) game

createVillage :: IO TileMap
createVillage = undefined

    where
        createRoadPoints :: Bool -> Int -> Point -> Point -> IO [Point]
        createRoadPoints False _ _ _ = return []
        createRoadPoints True 9 (xs, ys) (x, y) = do
            xc <- randomRIO(-1::Int, 1::Int)
            yc <- randomRIO(-1::Int, 1::Int)
            road <- createRoadPoints ((x + xs) < 40 && (y + ys) < 80) 0 (xs + xc, ys + yc) (x + xs, y + ys)
            return $ (x, y) : road
        createRoadPoints True a (xs, ys) (x, y) = do
            road <- createRoadPoints ((x + xs) < 40 && (y + ys) < 80) (a+1) (xs, ys) (x + xs, y + ys)
            return $ (x, y) : road

stringToWorldTileMap :: [String] -> WorldTileMap
stringToWorldTileMap []      = M.fromList []
stringToWorldTileMap mapdata = M.fromList $ map checkPlan [(x, y) | x <- [0..width], y <- [0..height]]
    where
        width = length (head mapdata) - 1
        height = (length mapdata) - 1

        checkPlan :: Point -> (Point, WorldMapTile)
        checkPlan p@(x, y) = (p, charToTile $ mapdata !! y !! x)

        charToTile :: Char -> WorldMapTile
        charToTile '&' = Forest
        charToTile '%' = Plains
        charToTile '^' = Mountain
        charToTile '_' = Lake
        charToTile '~' = River
        charToTile '/' = WorldRoad
        charToTile 'I' = TowerTile


worldmapTiles :: WorldTileMap
worldmapTiles = stringToWorldTileMap [
  -- 01234567890123456789012345678901234567890123456789012345678901234567890123456789
  -- 00000000001111111111222222222233333333334444444444555555555566666666667777777777
    "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^",-- 0
    "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^",-- 1
    "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^",-- 3
    "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^",-- 4
    "^^^^^^^^^^^^^^^^^%%%%%%%^^^^^^^^%%%%%%^^^^^^^^^^^^^^^^^^^^^^^%%%%%%%%%%^^^^^^^^^",-- 5
    "^^^^^^^^^^^^^^^^%%%%%%%%^^^^^^^^%%%%%%%%%^^^^^^^^^^^^^^^^^^%%%%%%%%%%%%^^^^^^^^^",-- 6
    "^^^^%%%%%%%%%%%%%%%%%%%%%%%^^^^%%%%%%%%%%%%^^^^^^^^^^^^^^^^%%%%%%%%%%%%%%%%^^^^^",-- 7
    "^^^^%%%%%%%%%%%%%%%%%%%%%%%^^^^%%%%%%%%%%%%^^^^^^^^^^^^^^^^%%%%%%%%%%%%%%%%%%%^^",-- 8
    "^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%^^%%%%%%%%%%%%%%%^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%",-- 9
    "^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%^^%%%%%%%%%%%%%%%^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%",-- 10
    "^^^^%%%%%%%%%I%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%^^^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%",-- 11
    "^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%^^^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%",-- 12
    "^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%^^^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%%",-- 13
    "^^%%%%%%%%%%%%%%%%%%&%%%%%%%%%%%%%%%%%%%^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",-- 14
    "^^%%%%%%%%%%%%%%%%&&%%%&%&%%%%%%%%^^^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",-- 15
    "^^%%%%%%%%%%%%%&&&&&%&&&%%%%%%%%%%%^^^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",-- 16
    "__%%%%%%%%%%%&%%&&&&&&&&&&&%&%%%%%%%%%^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",-- 17
    "__%%%%%%%%%%%%%&&&&%&&&&&&%%%%%%%%%%%%^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",-- 18
    "__%%%%%%%%%%%%&%%&&&&&%&&&%&&%%%%%%%%%%%%%^^^^%%%%%%%%%%%%%%%%%^^^^^^%%%%%%%%%%%",-- 19
    "__%%%%%%%%%%%%%%&&&&&%%&&%&&%%%%%%%%%%%%%^^^^%%%%%%%%%%%%%%%%%^^^^^^%%%%%%%%%%%%",-- 20
    "____%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%^^^^^^^^%%%%%%%%%%%%%%",-- 21
    "____%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%^^^^^^^^%%%%%%%%%%%%%%",-- 22
    "________%%%%%%%%%%%%%%%%%%%%%%%%%%%^^^^%%%^^%%%%%%%%%%%%%%%%^^%%%%%%%%%%%%%%%%%%",-- 23
    "________%%%%%%%%%%%%%%%%%%%%%%%%%%%^^^^%%%^^%%%%%%%%%%%%%%%%^^%%%%%%%%%%%%%%%%%%",-- 24
    "_______________%%%%%%%%%%%%%%%%%%%%%%%^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",-- 25
    "_______________%%%%%%%%%%%%%%%%%%%%%%%^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",-- 26
    "________________________%%%%%%%%%%%%%%%%^^%%%%%%%%%%%%%%%%%%%%____%%%%%%%%%%%%%%",-- 27
    "________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%_________%%%%%%%%%%%%%",-- 28
    "________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%________%%%%%%%%%%%%",-- 29
    "________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%____%%%%%%%%%%%%%%",-- 30
    "________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%%%%%%%%%%%%%%%%%%%",-- 31
    "__________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%%%%%%%%%%%%%%%%___",-- 32
    "__________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%%%%%%%%%%%%%%%%___",-- 33
    "__________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%~%%%%%%%%%%%%%%%___",-- 34
    "__________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%%%~%%%%%%%%%%%%%%___",-- 35
    "_____________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%%%%~%%%%%%%%_________",-- 36
    "_________________________________%%%%%%%%%%%%%%%%%%%%%%%%~______________________",-- 37
    "________________________________________________________________________________",-- 38
    "________________________________________________________________________________",-- 39
    "________________________________________________________________________________"]-- 40

newGame :: IO Game
newGame = return $ Game (Player "" (0,0) 0 100 100 [])
                        (Tower 0 0 0 0 0 60)
                        (replicate 5 Zombi)
                        worldmapTiles
                        (M.fromList [((20, 20), Village "Test village" 10 False),
                                     ((33, 30), Village "Another village" 15 False),
                                     ((32,  4), Village "Mountain village of Irongate" 18 False),
                                     ((24, 17), Village "Logging camp" 5 False),
                                     ((19, 13), Village "Small village" 9 False),
                                     ((42, 20), Village "Mountain town" 20 False),
                                     ((57, 34), Village "Town by the river" 25 False),
                                     ((74, 24), Village "Settlement in the plains" 15 False),
                                     ((54, 10), Village "A mountain village" 10 False),
                                     ((79, 10), Village "Border village" 15 False),
                                     ((62, 18), Village "A village by the mountain" 15 False),
                                     ((10, 23), Village "Small fishing village" 0 False),
                                     ((58, 26), Village "Laketown" 30 False),
                                     ((60, 25), Castle)]) --worldVillageMap
                        (10, 10)
                        (M.fromList [])
                        (M.fromList [((9,20), Guard),((10,20), King),((11,20), Male),((12,20), Child)])
                        (M.fromList [])
                        (M.fromList [])
                        []

{- 80 x 50
"%%%%%%%%%#######################################################################"
"%%%%%%%%%#....#...............................#......................#.........#"
"%%%%%%%%%#....#...##################..........+......................#.........#"
"%%%%%%%%%#....#...#................#..........#################+######.........#"
"%%%%%%%%%#....+...#................#..........#...........#..........+.........#"
"%%%%%%%%%######...#####+#######....#..........#...........#..........#.........#"
"%%%%%%%%%#....................#....#..........+...........############.........#"
"%%%%%%%%%#....................#....#..........#...........#..........+.........#"
"%%%%%%%#####..................######........################+###################"
"%%%%%%%#...#..............................###..................................#"
"%%%%%%%#...+............................###....................................#"
"%%%%%%%#...#..........................###......................................#"
"%%%%%%%#####..........................#.......#...#...#...#...#...#...#...#....#""
"%%%%%%%%%#............................#........................................#"
"%%%%%%%%%#...........................##........................................#"
"%%%%%%%%%#.....................########........................................#"
"%%%%%%%%%#....................##......#...#...#...#...#...#...#...#............#"
"%%%%%%%%%###..................#.......#........................................#"
"%%%%%%%%%G....................+.......+........................................#"
"%%%%%%%%%G....................+.......+........................................#"
"%%%%%%%%%###..................#.......#........................................#"
"%%%%%%%%%#....................##......#...#...#...#...#...#...#...#............#"
"%%%%%%%%%#.....................########........................................#"
"%%%%%%%%%#...........................##........................................#"
"%%%%%%%%%#............................#........................................#"
"%%%%%%%#####..........................#.......#...#...#...#...#...#...#...#....#"
"%%%%%%%#...#..........................###......................................#"
"%%%%%%%#...+............................###....................................#"
"%%%%%%%#...#..............................###..................................#"
"%%%%%%%#####................................################+###################"
"%%%%%%%%%#....................................#...#............................#"
"%%%%%%%%%#...#######+##.......................#...#............................#"
"%%%%%%%%%#...#........#.......................#...#............................#"
"%%%%%%%%%#...#........#.......................#...#............................#"
"%%%%%%%%%#...#........#.......................+...+............................#"
"%%%%%%%%%#...#........#...#####+###########...#...#............................#"
"%%%%%%%%%#...#........#...#...............#...#...#............................#"
"%%%%%%%%%#...##########...#...............#...#...#............................#"
"%%%%%%%%%#................#################...#...#............................#"
"%%%%%%%%%#....................................#...#............................#"
"%%%%%%%%%#######################################################################"
-}
