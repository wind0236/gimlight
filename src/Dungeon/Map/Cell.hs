{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Dungeon.Map.Cell
    ( CellMap
    , TileIdLayer(..)
    , Error(..)
    , upper
    , lower
    , cellMap
    , tileIdLayer
    , allWallTiles
    , changeTileAt
    , updateExploredMap
    , updatePlayerFov
    , playerFov
    , walkableFloors
    , transparentMap
    , exploredMap
    , widthAndHeight
    , isPositionInMap
    , locateActorAt
    , locateItemAt
    , removeActorAt
    , removeItemAt
    , removeActorIf
    , positionsAndActors
    , positionsAndItems
    , tileIdLayerAt
    ) where

import           Actor               (Actor, isPlayer)
import           Control.Lens        (Ixed (ix), makeLenses, (%%~), (%~), (&),
                                      (.~), (?~), (^.), (^?))
import           Control.Monad.State (StateT (StateT), gets)
import           Coord               (Coord)
import           Data.Array          (Array, array, assocs, bounds, (!), (//))
import           Data.Binary         (Binary)
import           Data.Foldable       (find)
import           Data.Maybe          (isJust, isNothing, mapMaybe)
import           Dungeon.Map.Tile    (TileCollection, TileId, floorTile,
                                      wallTile)
import qualified Dungeon.Map.Tile    as Tile
import           Fov                 (calculateFov)
import           GHC.Generics        (Generic)
import           Item                (Item)
import           Linear.V2           (V2 (V2))

data Error
    = OutOfRange
    | ActorNotFound
    | ActorAlreadyExists Actor
    | ItemNotFound
    | ItemAlreadyExists Item
    | TileIsNotWalkable
    deriving (Show)

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

isTileWalkable :: TileCollection -> Cell -> Bool
isTileWalkable tc c =
    fmap (Tile.isWalkable . (tc !)) (c ^. tileIdLayer . upper) /= Just False

locateActor :: TileCollection -> Actor -> Cell -> Either Error Cell
locateActor tc a c
    | not $ isTileWalkable tc c = Left TileIsNotWalkable
    | otherwise =
        case c ^. actor of
            Just x  -> Left $ ActorAlreadyExists x
            Nothing -> Right $ c & actor ?~ a

locateItem :: TileCollection -> Item -> Cell -> Either Error Cell
locateItem tc i c
    | not $ isTileWalkable tc c = Left TileIsNotWalkable
    | otherwise =
        case c ^. item of
            Just x  -> Left $ ItemAlreadyExists x
            Nothing -> Right $ c & item ?~ i

removeActor :: Cell -> (Either Error Actor, Cell)
removeActor c =
    case c ^. actor of
        Just a  -> (Right a, c & actor .~ Nothing)
        Nothing -> (Left ActorNotFound, c)

removeItem :: Cell -> (Either Error Item, Cell)
removeItem c =
    case c ^. item of
        Just i  -> (Right i, c & item .~ Nothing)
        Nothing -> (Left ItemNotFound, c)

-- We define `CellMap` as a newtype rather than an alias to define
-- functions that return `Nothing` on index-out-of-bounds error. Error
-- messages from `Array` are super unhelpful.
newtype CellMap =
    CellMap (Array (V2 Int) Cell)
    deriving (Show, Ord, Eq, Generic)

instance Binary CellMap

cellMap :: Array (V2 Int) TileIdLayer -> CellMap
cellMap = CellMap . fmap (\x -> Cell x Nothing Nothing False False)

allWallTiles :: V2 Int -> CellMap
allWallTiles (V2 width height) =
    CellMap $
    array
        (V2 0 0, V2 width height - V2 1 1)
        [(V2 x y, cell) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
  where
    cell =
        Cell
            (TileIdLayer (Just wallTile) (Just floorTile))
            Nothing
            Nothing
            False
            False

widthAndHeight :: CellMap -> V2 Int
widthAndHeight (CellMap m) = snd (bounds m) + V2 1 1

isPositionInMap :: Coord -> CellMap -> Bool
isPositionInMap (V2 x y) cm = x >= 0 && x < w && y >= 0 && y < h
  where
    V2 w h = widthAndHeight cm

changeTileAt ::
       (TileIdLayer -> TileIdLayer) -> Coord -> CellMap -> Maybe CellMap
changeTileAt f c (CellMap m)
    | isJust $ tileIdLayerAt c (CellMap m) =
        Just $ CellMap $ m // [(c, newTile)]
    | otherwise = Nothing
  where
    newTile = m ! c & tileIdLayer %~ f

walkableFloors :: TileCollection -> CellMap -> Array (V2 Int) Bool
walkableFloors tc (CellMap cm) = isWalkable tc <$> cm

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
             CellMap $ cm //
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

positionsAndActors :: CellMap -> [(Coord, Actor)]
positionsAndActors (CellMap cm) = mapMaybe mapStep $ assocs cm
  where
    mapStep (coord, cell) = (coord, ) <$> cell ^. actor

positionsAndItems :: CellMap -> [(Coord, Item)]
positionsAndItems (CellMap cm) = mapMaybe mapStep $ assocs cm
  where
    mapStep (coord, cell) = (coord, ) <$> cell ^. item

locateActorAt ::
       TileCollection -> Actor -> Coord -> StateT CellMap (Either Error) ()
locateActorAt tc a c =
    StateT $ \(CellMap cm) ->
        fmap (((), ) . CellMap) $ cm & ix c %%~ locateActor tc a

locateItemAt ::
       TileCollection -> Item -> Coord -> StateT CellMap (Either Error) ()
locateItemAt tc i c =
    StateT $ \(CellMap cm) ->
        fmap (((), ) . CellMap) $ cm & ix c %%~ locateItem tc i

removeActorAt :: Coord -> StateT CellMap (Either Error) Actor
removeActorAt c =
    StateT $ \(CellMap cm) ->
        case cm ^? ix c of
            Just x ->
                case removeActor x of
                    (Right removed, newCell) ->
                        Right (removed, CellMap $ cm // [(c, newCell)])
                    (Left e, _) -> Left e
            Nothing -> Left ActorNotFound

removeItemAt :: Coord -> StateT CellMap (Either Error) Item
removeItemAt c =
    StateT $ \(CellMap cm) ->
        case cm ^? ix c of
            Just x ->
                case removeItem x of
                    (Right removed, newCell) ->
                        Right (removed, CellMap $ cm // [(c, newCell)])
                    (Left e, _) -> Left e
            Nothing -> Left ItemNotFound

removeActorIf :: (Actor -> Bool) -> StateT CellMap (Either Error) Actor
removeActorIf f = do
    position <- gets $ fmap fst . find (f . snd) . positionsAndActors
    case position of
        Just x  -> removeActorAt x
        Nothing -> StateT $ const $ Left ActorNotFound

tileIdLayerAt :: Coord -> CellMap -> Maybe TileIdLayer
tileIdLayerAt c (CellMap cm) = cm ^? ix c . tileIdLayer
