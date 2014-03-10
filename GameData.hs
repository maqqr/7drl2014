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

data Tile = Floor
          | Road
          | WallWood
          | WallStone
          | Tree
          | Water
          | DoorClose | DoorOpen
          | Gate
          deriving (Eq, Show)

type Point = (Int, Int)

type TileMap = Map Point Tile

data Village = Village {
    villageName     :: String,
    size            :: Int,
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
         deriving (Eq, Show)

type NpcMap = Map Point Npc

data Game = Game {
    player          :: Player,
    tower           :: Tower,
    worldTileMap    :: WorldTileMap,
    worldVillageMap :: WorldVillageMap,

    minionMap       :: MinionMap,
    npcMap          :: NpcMap,
    tileMap         :: TileMap,
    corpseMap       :: CorpseMap,

    messageBuffer   :: [String]
}   deriving (Eq, Show)

addMsg :: String -> Game -> Game
addMsg str game
    | length buf >= maxBufferSize = game { messageBuffer = str : take (maxBufferSize-1) buf }
    | otherwise                   = game { messageBuffer = str : buf }
    where
        buf = messageBuffer game
        maxBufferSize = 5

--Funktiot--
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
            Just npc -> addMsg ("The missile strikes the" ++ show npc ++ ".") game

        target = M.lookup xy npcMap

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
        charToTile '#' = TowerTile


worldmapTiles :: WorldTileMap
worldmapTiles = stringToWorldTileMap [
    "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^",
    "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^",
    "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^",
    "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^",
    "^^^^^^^^^^^^^^^^%%%%%%%%^^^^^^^^%%%%%%%%%^^^^^^^^^^^^^^^^^^%%%%%%%%%%%%^^^^^^^^^",
    "^^^^^^^^^^^^^^^^%%%%%%%%^^^^^^^^%%%%%%%%%^^^^^^^^^^^^^^^^^^%%%%%%%%%%%%^^^^^^^^^",
    "^^^^%%%%%%%%%%%%%%%%%%%%%%%^^^^%%%%%%%%%%%%^^^^^^^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%",
    "^^^^%%%%%%%%%%%%%%%%%%%%%%%^^^^%%%%%%%%%%%%^^^^^^^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%",
    "^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%^^%%%%%%%%%%%%%%%^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%",
    "^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%^^%%%%%%%%%%%%%%%^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%",
    "^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%^^^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%",
    "^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%^^^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%",
    "^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%^^^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%%",
    "^^%%%%%%%%%%%%%%%%%%&%%%%%%%%%%%%%%%%%%%^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",
    "^^%%%%%%%%%%%%%%%%&&%%%&%&%%%%%%%%^^^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",
    "^^%%%%%%%%%%%%%&&&&&%&&&%%%%%%%%%%%^^^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",
    "__%%%%%%%%%%%&%%&&&&&&&&&&&%&%%%%%%%%%^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",
    "__%%%%%%%%%%%%%&&&&%&&&&&&%%%%%%%%%%%%^^^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",
    "__%%%%%%%%%%%%&%%&&&&&%&&&%&&%%%%%%%%%%%%%^^^^%%%%%%%%%%%%%%%%%^^^^^^%%%%%%%%%%%",
    "__%%%%%%%%%%%%%%&&&&&%%&&%&&%%%%%%%%%%%%%^^^^%%%%%%%%%%%%%%%%%^^^^^^%%%%%%%%%%%%",
    "____%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%^^^^^^^^%%%%%%%%%%%%%%",
    "____%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%^^^^^^^^%%%%%%%%%%%%%%",
    "________%%%%%%%%%%%%%%%%%%%%%%%%%%%^^^^%%%^^%%%%%%%%%%%%%%%%^^%%%%%%%%%%%%%%%%%%",
    "________%%%%%%%%%%%%%%%%%%%%%%%%%%%^^^^%%%^^%%%%%%%%%%%%%%%%^^%%%%%%%%%%%%%%%%%%",
    "_______________%%%%%%%%%%%%%%%%%%%%%%%^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",
    "_______________%%%%%%%%%%%%%%%%%%%%%%%^^^^^^^^%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",
    "________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%____%%%%%%%%%%%%%%",
    "________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%_________%%%%%%%%%%%%%",
    "________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%________%%%%%%%%%%%%",
    "________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%____%%%%%%%%%%%%%%",
    "________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%%%%%%%%%%%%%%%%%%%",
    "__________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%%%%%%%%%%%%%%%%___",
    "__________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%%%%%%%%%%%%%%%%___",
    "__________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%~%%%%%%%%%%%%%%%___",
    "__________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%%%~%%%%%%%%%%%%%%___",
    "_____________________________%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%%%%~%%%%%%%%_________",
    "_________________________________%%%%%%%%%%%%%%%%%%%%%%%%~______________________",
    "________________________________________________________________________________",
    "________________________________________________________________________________",
    "________________________________________________________________________________"]

newGame :: IO Game
newGame = return $ Game (Player "" (0,0) 0 100 100 [])
                        (Tower 0 0 0 0 0 0)
                        worldmapTiles
                        (M.fromList []) --worldVillageMap
                        (M.fromList [])
                        (M.fromList [])
                        (M.fromList [])
                        (M.fromList [])
                        []

-- Testing

testGame :: Game
testGame = Game (Player "" (0,0) 0 100 100 [])
                (Tower 0 0 0 0 0 0)
                (M.fromList []) --worldTileMap
                (M.fromList []) --worldVillageMap
                (M.fromList []) -- minion
                (M.fromList [((1,1), Male)]) -- npc
                (M.fromList []) -- corpse
                (M.fromList []) -- tile
                []
