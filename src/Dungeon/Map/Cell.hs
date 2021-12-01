{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Dungeon.Map.Cell
    ( CellMap
    , TileIdLayer(..)
    , upper
    , lower
    , cellMap
    , allWallTiles
    , changeTileAt
    , updateExploredMap
    , updatePlayerFov
    , playerFov
    , walkableMap
    , transparentMap
    , exploredMap
    , widthAndHeight
    , isWalkableAt
    , locateActorAt
    , locateItemAt
    , removeActorAt
    , removeItemAt
    , removeActorIf
    , positionsAndActors
    , positionsAndItems
    , tileIdLayerAt
    , cellAt
    ) where

import           Actor            (Actor, isPlayer)
import           Control.Lens     (makeLenses, (%~), (&), (.~), (?~), (^.))
import           Coord            (Coord)
import           Data.Array       (Array, assocs, bounds, (!), (//))
import           Data.Binary      (Binary)
import           Data.Foldable    (find)
import           Data.Maybe       (isJust, isNothing, mapMaybe)
import qualified Dungeon.Map      as M
import           Dungeon.Map.Fov  (calculateFov)
import           Dungeon.Map.Tile (TileCollection, TileId, floorTile, wallTile)
import qualified Dungeon.Map.Tile as Tile
import           GHC.Generics     (Generic)
import           Item             (Item)
import           Linear.V2        (V2 (V2))

data TileIdLayer =
    TileIdLayer
        { _upper :: Maybe TileId
        , _lower :: Maybe TileId
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''TileIdLayer

instance Binary TileIdLayer

data Cell =
    Cell
        { _tileIdLayer       :: TileIdLayer
        , _actor             :: Maybe Actor
        , _item              :: Maybe Item
        , _explored          :: Bool
        , _visibleFromPlayer :: Bool
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''Cell

instance Binary Cell

isWalkable :: TileCollection -> Cell -> Bool
isWalkable tc c =
    fmap (Tile.isWalkable . (tc !)) (c ^. (tileIdLayer . upper)) /= Just False &&
    isNothing (c ^. actor)

isTransparent :: TileCollection -> Cell -> Bool
isTransparent tc c =
    fmap (Tile.isTransparent . (tc !)) (c ^. (tileIdLayer . upper)) /=
    Just False

locateActor :: Actor -> Cell -> Maybe Cell
locateActor a c
    | isJust (c ^. actor) = Nothing
    | otherwise = Just $ c & actor ?~ a

locateItem :: Item -> Cell -> Maybe Cell
locateItem i c
    | isJust (c ^. item) = Nothing
    | otherwise = Just $ c & item ?~ i

removeActor :: Cell -> Maybe (Actor, Cell)
removeActor c =
    case c ^. actor of
        Just a  -> Just (a, c & actor .~ Nothing)
        Nothing -> Nothing

removeItem :: Cell -> Maybe (Item, Cell)
removeItem c =
    case c ^. item of
        Just i  -> Just (i, c & item .~ Nothing)
        Nothing -> Nothing

newtype CellMap =
    CellMap (Array (V2 Int) Cell)
    deriving (Show, Ord, Eq, Generic)

instance Binary CellMap

cellMap :: Array (V2 Int) TileIdLayer -> CellMap
cellMap = CellMap . fmap (\x -> Cell x Nothing Nothing False False)

allWallTiles :: V2 Int -> CellMap
allWallTiles wh =
    CellMap $
    M.generate
        wh
        (const
             (Cell
                  (TileIdLayer (Just wallTile) (Just floorTile))
                  Nothing
                  Nothing
                  False
                  False))

widthAndHeight :: CellMap -> V2 Int
widthAndHeight (CellMap m) = snd (bounds m) + V2 1 1

changeTileAt ::
       (TileIdLayer -> TileIdLayer) -> Coord -> CellMap -> Maybe CellMap
changeTileAt f c (CellMap m)
    | isJust $ tileIdLayerAt c (CellMap m) =
        Just $ CellMap $ m // [(c, newTile)]
    | otherwise = Nothing
  where
    newTile = m ! c & tileIdLayer %~ f

walkableMap :: TileCollection -> CellMap -> Array (V2 Int) Bool
walkableMap tc (CellMap cm) = isWalkable tc <$> cm

exploredMap :: CellMap -> Array (V2 Int) Bool
exploredMap (CellMap cm) = (^. explored) <$> cm

playerFov :: CellMap -> Array (V2 Int) Bool
playerFov (CellMap cm) = (^. visibleFromPlayer) <$> cm

transparentMap :: TileCollection -> CellMap -> Array (V2 Int) Bool
transparentMap tc (CellMap cm) = isTransparent tc <$> cm

updateExploredMap :: CellMap -> CellMap
updateExploredMap (CellMap cm) =
    CellMap $ cm // [(pos, cm ! pos & explored .~ True) | pos <- visibleList]
  where
    visibleList = map fst $ filter snd $ assocs $ playerFov $ CellMap cm

updatePlayerFov :: TileCollection -> CellMap -> Maybe CellMap
updatePlayerFov tc (CellMap cm) =
    fmap
        (\xs ->
             CellMap $
             cm //
             [ (pos, cm ! pos & visibleFromPlayer .~ isVisible)
             | (pos, isVisible) <- xs
             ])
        visibilityList
  where
    visibilityList = fmap assocs fov
    fov =
        (\x -> calculateFov x (transparentMap tc (CellMap cm))) <$>
        playerPosition
    playerPosition =
        fmap fst $ find (isPlayer . snd) $ positionsAndActors $ CellMap cm

isWalkableAt :: Coord -> TileCollection -> CellMap -> Bool
isWalkableAt c tc t =
    case cellAt c t of
        Just x  -> isWalkable tc x
        Nothing -> False

positionsAndActors :: CellMap -> [(Coord, Actor)]
positionsAndActors (CellMap cm) = mapMaybe mapStep $ assocs cm
  where
    mapStep (coord, cell) = (coord, ) <$> cell ^. actor

positionsAndItems :: CellMap -> [(Coord, Item)]
positionsAndItems (CellMap cm) = mapMaybe mapStep $ assocs cm
  where
    mapStep (coord, cell) = (coord, ) <$> cell ^. item

locateActorAt :: Actor -> Coord -> CellMap -> Maybe CellMap
locateActorAt a c (CellMap cm)
    | coordIsInRange c (CellMap cm) =
        (\x -> CellMap (cm // [(c, x)])) <$> newCell
    | otherwise = Nothing
  where
    newCell = locateActor a (cm ! c)

locateItemAt :: Item -> Coord -> CellMap -> Maybe CellMap
locateItemAt i c (CellMap cm)
    | coordIsInRange c (CellMap cm) =
        (\x -> CellMap (cm // [(c, x)])) <$> newCell
    | otherwise = Nothing
  where
    newCell = locateItem i (cm ! c)

removeActorAt :: Coord -> CellMap -> Maybe (Actor, CellMap)
removeActorAt c (CellMap cm) =
    case cellAt c (CellMap cm) >>= removeActor of
        Just (a, newCell) -> Just (a, CellMap $ cm // [(c, newCell)])
        Nothing           -> Nothing

removeItemAt :: Coord -> CellMap -> Maybe (Item, CellMap)
removeItemAt c (CellMap cm) =
    case cellAt c (CellMap cm) >>= removeItem of
        Just (a, newCell) -> Just (a, CellMap $ cm // [(c, newCell)])
        Nothing           -> Nothing

removeActorIf :: (Actor -> Bool) -> CellMap -> Maybe (Actor, CellMap)
removeActorIf f cm = position >>= flip removeActorAt cm
  where
    position = fst <$> find (f . snd) (positionsAndActors cm)

tileIdLayerAt :: Coord -> CellMap -> Maybe TileIdLayer
tileIdLayerAt c t = fmap (^. tileIdLayer) (cellAt c t)

cellAt :: Coord -> CellMap -> Maybe Cell
cellAt c (CellMap m)
    | coordIsInRange c (CellMap m) = Just $ m ! c
    | otherwise = Nothing

coordIsInRange :: Coord -> CellMap -> Bool
coordIsInRange c (CellMap m) = c >= lowerBound && c <= upperBound
  where
    (lowerBound, upperBound) = bounds m
