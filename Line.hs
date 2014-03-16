{-# LANGUAGE ExplicitForAll, ScopedTypeVariables #-}
module Line(
	line,
	lineOfSight
) where

import GameData (Point)

-- | Bresenham algorithm
--
-- Original code by Chris Dew
-- http://www.barricane.com/2009/09/21/thoughts-on-bresenhams-algorithm-in-haskell.html
bres :: Int -> Int -> [Point]
bres run rise
    | run  <  0  = [(-x, y) | (x, y) <- bres (-run) rise]
    | rise <  0  = [(x, -y) | (x, y) <- bres run (-rise)]
    | rise > run = [(x,  y) | (y, x) <- bres rise run   ]
    | otherwise  = zip [0..run] . map fst $ iterate step (0, run `div` 2)
    where
        step (y, err)
            | error' < 0 = (y + 1, error' + run)
            | otherwise  = (y,     error')
            where error' = err - rise

line :: Point -> Point -> [Point]
line (x1, y1) (x2, y2) = [(x1+x, y1+y) | (x, y) <- bres (x2-x1) (y2-y1)]


-- | Calculates line of sight
lineOfSight :: (Point -> Bool)
            -> Point
            -> Point
            -> Bool
lineOfSight isBlocked start end = checkLine . drop 1 $ line start end
    where
        checkLine :: [Point] -> Bool
        checkLine [] = True
        checkLine (_:[]) = True
        checkLine (p:oints)
            | isBlocked p = False
            | otherwise = checkLine oints
