{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Dungeon.Map.Cell
    ( CellMap
    , cellMap
    , allWallTiles
    , changeTileAt
    , updateExploredMap
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
    , tileIdAt
    , cellAt
    ) where

import           Actor            (Actor)
import           Control.Lens     (makeLenses, (&), (.~), (?~), (^.))
import           Coord            (Coord)
import           Data.Array       (Array, assocs, bounds, (!), (//))
import           Data.Binary      (Binary)
import           Data.Foldable    (find)
import           Data.Maybe       (isJust, isNothing, mapMaybe)
import qualified Dungeon.Map      as M
import           Dungeon.Map.Bool (BoolMap)
import           Dungeon.Map.Fov  (Fov)
import           Dungeon.Map.Tile (TileCollection, TileId, wallTile)
import qualified Dungeon.Map.Tile as Tile
import           GHC.Generics     (Generic)
import           Item             (Item)
import           Linear.V2        (V2 (V2))

data Cell =
    Cell
        { _tileId   :: TileId
        , _actor    :: Maybe Actor
        , _item     :: Maybe Item
        , _explored :: Bool
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''Cell

instance Binary Cell

isWalkable :: TileCollection -> Cell -> Bool
isWalkable tc c = Tile.isWalkable (tc ! (c ^. tileId)) && isNothing (c ^. actor)

isTransparent :: TileCollection -> Cell -> Bool
isTransparent tc c = Tile.isTransparent (tc ! (c ^. tileId))

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

cellMap :: Array (V2 Int) TileId -> CellMap
cellMap = CellMap . fmap (\x -> Cell x Nothing Nothing False)

allWallTiles :: V2 Int -> CellMap
allWallTiles wh =
    CellMap $ M.generate wh (const (Cell wallTile Nothing Nothing False))

widthAndHeight :: CellMap -> V2 Int
widthAndHeight (CellMap m) = snd (bounds m) + V2 1 1

changeTileAt :: Coord -> TileId -> CellMap -> Maybe CellMap
changeTileAt c i (CellMap m)
    | isJust $ tileIdAt c (CellMap m) = Just $ CellMap $ m // [(c, newTile)]
    | otherwise = Nothing
  where
    newTile = m ! c & tileId .~ i

walkableMap :: TileCollection -> CellMap -> BoolMap
walkableMap tc (CellMap cm) = isWalkable tc <$> cm

exploredMap :: CellMap -> BoolMap
exploredMap (CellMap cm) = (^. explored) <$> cm

transparentMap :: TileCollection -> CellMap -> BoolMap
transparentMap tc (CellMap cm) = isTransparent tc <$> cm

updateExploredMap :: Fov -> CellMap -> CellMap
updateExploredMap fov (CellMap cm) =
    CellMap $ cm // [(pos, (cm ! pos) & explored .~ True) | pos <- visibleList]
  where
    visibleList = map fst $ filter snd $ assocs fov

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

tileIdAt :: Coord -> CellMap -> Maybe TileId
tileIdAt c t = (^. tileId) <$> cellAt c t

cellAt :: Coord -> CellMap -> Maybe Cell
cellAt c (CellMap m)
    | coordIsInRange c (CellMap m) = Just $ m ! c
    | otherwise = Nothing

coordIsInRange :: Coord -> CellMap -> Bool
coordIsInRange c (CellMap m) = c >= lower && c <= upper
  where
    (lower, upper) = bounds m
