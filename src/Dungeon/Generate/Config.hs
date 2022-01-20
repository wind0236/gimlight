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
    , roomMinIsLargerThanRoomMax
    , roomMaxSizeIsLargerThanRoomWidth
    , roomMaxSizeIsLargerThanRoomHeight
    ) where

import           Linear.V2 (V2 (V2))

data Config =
    Config
        { numOfFloors :: Int
        , maxRooms    :: Int
        , roomMinSize :: Int
        , roomMaxSize :: Int
        , mapSize     :: V2 Int
        }
    deriving (Show)

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
config nf mr rmin rmax ms@(V2 width height)
    | nf <= 0 = error numOfFloorsMustBePositive
    | mr <= 0 = error maxRoomMustBePositive
    | rmin <= 0 = error roomMinSizeMustBePositive
    | rmin > rmax = error $ roomMinIsLargerThanRoomMax rmin rmax -- No need to check if `rmax <= 0` as this ensures that `0 < rmin <= rmax`.
    | rmax > width = error $ roomMaxSizeIsLargerThanRoomWidth rmax width
    | rmax > height = error $ roomMaxSizeIsLargerThanRoomHeight rmax height
    | otherwise = Config nf mr rmin rmax ms

numOfFloorsMustBePositive :: String
numOfFloorsMustBePositive = "The number of floors must be positive."

maxRoomMustBePositive :: String
maxRoomMustBePositive = "The maximum number of rooms must be positive."

roomMinSizeMustBePositive :: String
roomMinSizeMustBePositive = "The minimum room size must be positive."

roomMinIsLargerThanRoomMax :: Int -> Int -> String
roomMinIsLargerThanRoomMax rmin rmax =
    "The room minimum size " ++
    show rmin ++
    " is larger than or equal to the room maximum size " ++ show rmax ++ "."

roomMaxSizeIsLargerThanRoomWidth :: Int -> Int -> String
roomMaxSizeIsLargerThanRoomWidth rmax width =
    "The room maximum size " ++
    show rmax ++
    " is larger than or equal to the map width " ++ show width ++ "."

roomMaxSizeIsLargerThanRoomHeight :: Int -> Int -> String
roomMaxSizeIsLargerThanRoomHeight rmax height =
    "The room maximum size " ++
    show rmax ++
    " is larger than or equal to the map height " ++ show height ++ "."
