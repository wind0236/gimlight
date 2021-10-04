module Map.Fov
    ( Fov
    , initFov
    , calculateFov
    ) where

import           Coord        (Coord)
import           Data.Array   ((!), (//))
import           Dungeon.Size (height, width)
import           Linear.V2    (V2 (..))
import           Map.Bool     (BoolMap, emptyBoolMap)

type Fov = BoolMap

fovRadius :: Int
fovRadius = 8

initFov :: Fov
initFov = emptyBoolMap

calculateFov :: Coord -> BoolMap -> Fov
calculateFov (V2 x0 y0) transparentMap =
        foldl (flip (calculateLos transparentMap (V2 x0 y0))) emptyBoolMap
            [V2 (x0 + x) (y0 + y) |
            x <- [(-fovRadius) .. fovRadius],
            y <- [(-fovRadius) .. fovRadius]
            ]

calculateLos :: BoolMap -> Coord -> Coord -> Fov -> Fov
calculateLos m p0 = calculateLosAccum p0 m p0

calculateLosAccum :: Coord -> BoolMap -> Coord -> Coord -> BoolMap -> BoolMap
calculateLosAccum (V2 xnext ynext) transparentMap (V2 x0 y0) (V2 x1 y1) fov
        | x1 < 0 || y1 < 0 || x1 >= width || y1 >= height = fov
        | V2 xnext ynext == V2 x1 y1 = fov // [((x1, y1), True)]
        | not $ transparentMap ! (xnext, ynext) = fov
        | fromIntegral(abs(dy * (xnext - x0 + sx) - dx * (ynext - y0))) / dist < 0.5 =
            calculateLosAccum (V2 (xnext + sx) ynext) transparentMap (V2 x0 y0) (V2 x1 y1) fov
        | fromIntegral(abs(dy * (xnext - x0) - dx * (ynext - y0 + sy))) / dist < 0.5 =
            calculateLosAccum (V2 xnext (ynext + sy)) transparentMap (V2 x0 y0) (V2 x1 y1) fov
        | otherwise =
            calculateLosAccum (V2 (xnext + sx) (ynext + sy)) transparentMap (V2 x0 y0) (V2 x1 y1) fov
            where dx = x1 - x0
                  dy = y1 - y0
                  sx = if x0 < x1 then 1 else -1
                  sy = if y0 < y1 then 1 else -1
                  dist = sqrt $ fromIntegral $ dx * dx + dy * dy :: Float
