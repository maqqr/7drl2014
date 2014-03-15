module TownGenerator where

import Data.Traversable
import qualified Data.Foldable as F
import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import System.Random (randomRIO, randomIO)

import GameData

type Rect = (Int, Int, Int, Int)

-- | Binary space partition
data Bsp a = HBranch !Float (Bsp a) (Bsp a) -- split from left to right
           | VBranch !Float (Bsp a) (Bsp a) -- ´split from top to bottom
           | Leaf a deriving (Eq, Show)

instance Functor Bsp where
    fmap f (HBranch s top bottom) = HBranch s (fmap f top) (fmap f bottom)
    fmap f (VBranch s left right) = VBranch s (fmap f left) (fmap f right)
    fmap f (Leaf x) = Leaf $ f x

instance F.Foldable Bsp where
    foldr f z (Leaf x) = f x z
    foldr f z (HBranch _ left right) = F.foldr f (F.foldr f z right) left
    foldr f z (VBranch _ left right) = F.foldr f (F.foldr f z right) left

instance Traversable Bsp where
    traverse f (Leaf x) = Leaf <$> f x
    traverse f (HBranch s left right) = HBranch <$> pure s <*> traverse f left <*> traverse f right
    traverse f (VBranch s left right) = VBranch <$> pure s <*> traverse f left <*> traverse f right


-- | Returns a random float between [0.0, 1.0]
randomFloat :: IO Float
randomFloat = randomRIO (0, 1000) `dividedBy` pure 1000.0
    where
        dividedBy = liftA2 (/)


-- | Returns random boolean
randomBool :: IO Bool
randomBool = (==) <$> pure 1 <*> randomRIO (0, 1 :: Int)


-- | Splits Int
--
-- >>> splitInt 10 0.3
-- (3,7)
splitInt :: Int -> Float -> Point
splitInt x s = let x' = truncate (fromIntegral x * s) in (x', x - x')


-- | Splits Rect into two parts
--
-- Horizontal split:
-- 
-- >>> splitRect 0.3 True (0, 0, 10, 7)
-- ((0,0,10,2),(0,2,10,5))
--
-- Vertical split:
-- 
-- >>> splitRect 0.3 False (0, 0, 10, 7)
-- ((0,0,3,7),(3,0,7,7))
splitRect :: Float -> Bool -> Rect -> (Rect, Rect)
splitRect split True  (x,y,w,h) =
    let (upper,lower) = splitInt h split in ( (x,y,w,upper), (x,y+upper,w,lower) )
splitRect split False (x,y,w,h) =
    let (left,right) = splitInt w split in ( (x,y,left,h), (x+left,y,right,h) )


-- | Splits Bsp Rect randomly
splitBsp :: (Rect -> Bool) -> Bsp Rect -> IO (Bsp Rect)
splitBsp p (HBranch s top bottom) = do
    top' <- splitBsp p top
    bottom' <- splitBsp p bottom
    return $ HBranch s top' bottom'
splitBsp p (VBranch s top bottom) = do
    top' <- splitBsp p top
    bottom' <- splitBsp p bottom
    return $ VBranch s top' bottom'
splitBsp p (Leaf r) = do
    split <- liftM ((+)0.3 . (*)0.4) randomFloat
    horizontal <- randomBool
    let (leftr, rightr) = splitRect split horizontal r
    if p leftr && p rightr then do
            --left <- splitBsp p (Leaf leftr)
            --right <- splitBsp p (Leaf rightr)
            let branch = if horizontal then HBranch else VBranch
            return $ branch split (Leaf leftr) (Leaf rightr)
        else return (Leaf r)


-- | Generates randomly split Bsp Rect
genBsp :: Rect -> IO (Bsp Rect)
genBsp initialRect = splitter
    where
        splitter :: IO (Bsp Rect)
        splitter = fix (6 :: Integer) (splitBsp goodRoom) (Leaf initialRect)
        
        fix 0 f x = f x
        fix n f x = f =<< fix (n-1) f x

        goodRoom (_, _, w, h) = w >= minRoom && h >= minRoom
        minRoom = 10


-- | Generates random village map
randomVillageMap :: Rect -> Int -> IO (TileMap, NpcMap)
randomVillageMap (sx, sy, w, h) size = do
    bsp <- genBsp (sx, sy, w-1, h-1)
    let emptyMap = M.fromList [((x, y), Grass) | x <- [sx..w-1], y <- [sy..h-1]]
    bspWithHouses <- generateHouses bsp
    let tilemap = buildHouses emptyMap . ptrace . F.foldr (:) [] $ bspWithHouses
    potentialNpcList <- potentialNpcs
    return (tilemap, M.fromList potentialNpcList)
    where
        minHouseSize = 5

        buildHouses :: TileMap -> [(Rect, Rect)] -> TileMap
        buildHouses = foldr buildHouse

        buildHouse :: (Rect, Rect) -> TileMap -> TileMap
        buildHouse ((areax, areay, areaw, areah),(x',y',w',h')) = (flip . foldr . uncurry $ M.insert) houseTiles
            where
                houseTiles = [((x, y), selectTile (x, y)) | x <- [x'..x'+w'], y <- [y'..y'+h']] ++ road
                selectTile (x, y)
                    | y == y' && x == x' + (w' `quot` 2) = DoorClose
                    | x == x'+w' || y == y'+h' || x == x' || y == y' = WallWood
                    | otherwise = Floor
                road = [((x, areay+areah), Road) | x <- [areax..areax+areaw]] ++ [((areax+areaw, y), Road) | y <- [areay..areay+areah]]

        generateHouses :: Bsp Rect -> IO (Bsp (Rect, Rect))
        generateHouses = traverse planHouse

        planHouse :: Rect -> IO (Rect, Rect)
        planHouse rect@(x',y',w',h') = do
            rw <- randomRIO (minHouseSize, w' - 4)
            rh <- randomRIO (minHouseSize, h' - 4)
            rx <- randomRIO (x'+1, x' + (w' - rw - 2))
            ry <- randomRIO (y'+1, y' + (h' - rh - 2))
            let generatedRoom = (rx, ry, rw, rh)
            return (rect, generatedRoom)

        randomPoint :: IO Point
        randomPoint = (,) <$> randomRIO (0, w-1) <*> randomRIO (0, h-1)

        potentialNpc :: IO (Point, Npc)
        potentialNpc = (,) <$> randomPoint <*> randomIO

        potentialNpcs :: IO [(Point, Npc)]
        potentialNpcs = replicateM size potentialNpc