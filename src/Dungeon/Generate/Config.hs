module Dungeon.Generate.Config
    ( Config
    , getNumOfFloors
    , getMaxRooms
    , getRoomMinSize
    , getRoomMaxSize
    , getMapSize
    , config
    , numOfFloorsMustBePositive
    , maxRoomMustBePositive
    , roomMinSizeMustBePositive
    , mapWidthIsTooSmall
    , mapHeightIsTooSmall
    , roomMinIsLargerThanRoomMax
    ) where

import           Linear.V2      (V2 (V2))
import           UI.Draw.Config (tileColumns, tileRows)

data Config =
    Config
        { numOfFloors :: Int
        , maxRooms    :: Int
        , roomMinSize :: Int
        , roomMaxSize :: Int
        , mapSize     :: V2 Int
        }

getNumOfFloors :: Config -> Int
getNumOfFloors = numOfFloors

getMaxRooms :: Config -> Int
getMaxRooms = maxRooms

getRoomMinSize :: Config -> Int
getRoomMinSize = roomMinSize

getRoomMaxSize :: Config -> Int
getRoomMaxSize = roomMaxSize

getMapSize :: Config -> V2 Int
getMapSize = mapSize

config :: Int -> Int -> Int -> Int -> V2 Int -> Config
config nf mr rmin rmax ms@(V2 mw mh)
    | nf <= 0 = error numOfFloorsMustBePositive
    | mr <= 0 = error maxRoomMustBePositive
    | rmin <= 0 = error roomMinSizeMustBePositive
    | mw < tileColumns = error $ mapWidthIsTooSmall mw
    | mh < tileRows = error $ mapHeightIsTooSmall mh
    | rmin > rmax = error $ roomMinIsLargerThanRoomMax rmin rmax -- No need to check if `rmin <= 0` as this ensures that `0 < rmin < rmax`.
    | otherwise = Config nf mr rmin rmax ms

numOfFloorsMustBePositive :: String
numOfFloorsMustBePositive = "The number of floors must be positive."

maxRoomMustBePositive :: String
maxRoomMustBePositive = "The maximum number of rooms must be positive."

roomMinSizeMustBePositive :: String
roomMinSizeMustBePositive = "The minimum room size must be positive."

mapWidthIsTooSmall :: Int -> String
mapWidthIsTooSmall w =
    "Map width is expected to be larger than or equal to " ++
    show tileColumns ++ " but the actual value is " ++ show w ++ "."

mapHeightIsTooSmall :: Int -> String
mapHeightIsTooSmall h =
    "Map height is expected to be larger than or equal to " ++
    show tileColumns ++ " but the actual value is " ++ show h ++ "."

roomMinIsLargerThanRoomMax :: Int -> Int -> String
roomMinIsLargerThanRoomMax rmin rmax =
    "The room minimum size " ++
    show rmin ++
    " is larger than or equal to the room maximum size " ++ show rmax ++ "."
