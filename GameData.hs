{-# LANGUAGE RecordWildCards #-}
module GameData where

import qualified Data.Map as M
import Data.Map ((\\),Map)
import System.Random
import Data.Maybe

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
    corpseMap       :: CorpseMap
}   deriving (Eq, Show)

--Funktiot--

riseUndead :: Game -> Game
riseUndead old@Game {..} = old {corpseMap = corpseMap \\ nearCorpses,
                                minionMap = M.union minionMap $ M.map (\corpses -> map convertToUndead corpses) nearCorpses}
    where   distance :: Point -> Point -> Int
            distance (x,y) (xs,ys) = (x - xs)^2 + (y - ys)^2

            convertToUndead :: Corpse -> Zombi
            convertToUndead (Corpse Guard) = GuardZombi
            convertToUndead (Corpse King ) = KingZombi
            convertToUndead (Corpse _    ) = Zombi

            nearCorpses = M.filterWithKey (\xy _ -> distance xy (place player) <= 5^2) corpseMap

drainLife :: Game -> Point -> IO Game
drainLife old@Game {..} xy = do
    luku <- randomRIO (0::Int, 101::Int)
    if luku < 50
        then return $ old {corpseMap = M.alter (convertToCorpse target) xy corpseMap}

        else return old

    where   target = M.lookup xy npcMap

            convertToCorpse :: Maybe Npc -> Maybe [Corpse] -> Maybe [Corpse]
            convertToCorpse Nothing Nothing = Nothing
            convertToCorpse Nothing oldies = oldies
            convertToCorpse (Just npc) Nothing = Just [Corpse npc]
            convertToCorpse (Just npc) (Just oldies) = Just (Corpse npc:oldies)


stringToWorldTileMap :: [String] -> WorldTileMap
stringToWorldTileMap mapdata = M.fromList $ map checkPlan [(x, y) | x <- [0..width], y <- [0..height]]
    where
        width = length (head mapdata) - 1
        height = (length mapdata) - 1

        checkPlan :: Point -> (Point, WorldMapTile)
        checkPlan p@(x, y) = (p, charToTile $ mapdata !! y !! x)

        charToTile :: Char -> WorldMapTile
        charToTile '&' = Forest
        charToTile '"' = Plains
        charToTile '^' = Mountain
        charToTile '_' = Lake
        charToTile '~' = River
        charToTile '/' = WorldRoad
        charToTile '#' = TowerTile

newGame :: String -> IO Game
newGame name = return $ Game  (Player name (0,0) 0 100 100 [])
                              (Tower 0 0 0 0 0 0)
                            (M.fromList  []) --worldTileMap
                            (M.fromList  []) --worldVillageMap
                            (M.fromList  [])
                            (M.fromList  [])
                            (M.fromList  [])
                            (M.fromList  [])
