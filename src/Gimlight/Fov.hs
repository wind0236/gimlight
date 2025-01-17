module Gimlight.Fov
    ( Fov
    , calculateFov
    ) where

import           Data.Array     (Array, array, bounds, (!), (//))
import           Gimlight.Coord (Coord)
import           Linear.V2      (V2 (..))

type Fov = Array (V2 Int) Bool

fovRadius :: Int
fovRadius = 8

calculateFov :: Coord -> Array (V2 Int) Bool -> Fov
calculateFov src transparentMap =
    foldl
        (flip (calculateLos transparentMap src))
        (darkFov $ snd (bounds transparentMap) + V2 1 1)
        [ src + V2 x y
        | x <- [(-fovRadius) .. fovRadius]
        , y <- [(-fovRadius) .. fovRadius]
        ]

calculateLos :: Array (V2 Int) Bool -> Coord -> Coord -> Fov -> Fov
calculateLos m p0 = calculateLosAccum p0 m p0

calculateLosAccum ::
       Coord
    -> Array (V2 Int) Bool
    -> Coord
    -> Coord
    -> Array (V2 Int) Bool
    -> Array (V2 Int) Bool
calculateLosAccum (V2 xnext ynext) transparentMap (V2 x0 y0) (V2 x1 y1) fov
    | x1 < 0 || y1 < 0 || x1 >= width || y1 >= height = fov
    | V2 xnext ynext == V2 x1 y1 = fov // [(V2 x1 y1, True)]
    | not $ transparentMap ! V2 xnext ynext = fov
    | fromIntegral (abs (dy * (xnext - x0 + sx) - dx * (ynext - y0))) / dist <
          0.5 =
        calculateLosAccum
            (V2 (xnext + sx) ynext)
            transparentMap
            (V2 x0 y0)
            (V2 x1 y1)
            fov
    | fromIntegral (abs (dy * (xnext - x0) - dx * (ynext - y0 + sy))) / dist <
          0.5 =
        calculateLosAccum
            (V2 xnext (ynext + sy))
            transparentMap
            (V2 x0 y0)
            (V2 x1 y1)
            fov
    | otherwise =
        calculateLosAccum
            (V2 (xnext + sx) (ynext + sy))
            transparentMap
            (V2 x0 y0)
            (V2 x1 y1)
            fov
  where
    dx = x1 - x0
    dy = y1 - y0
    sx
        | x0 < x1 = 1
        | otherwise = -1
    sy
        | y0 < y1 = 1
        | otherwise = -1
    dist = sqrt $ fromIntegral $ dx * dx + dy * dy :: Float
    V2 width height = snd (bounds fov) + V2 1 1

darkFov :: V2 Int -> Array (V2 Int) Bool
darkFov (V2 width height) =
    array
        (V2 0 0, V2 width height - V2 1 1)
        [(V2 x y, False) | y <- [0 .. height - 1], x <- [0 .. width - 1]]
