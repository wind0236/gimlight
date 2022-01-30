{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Gimlight.Dungeon.Map.Cell
    ( CellMap
    , TileIdentifierLayer(..)
    , Error(..)
    , tileIdentifierLayer
    , upper
    , lower
    , cellMap
    , allWallTiles
    , changeTileAt
    , updateExploredMap
    , updatePlayerFov
    , playerFov
    , playerActor
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
    , tileIdentifierLayerAt
    ) where

import           Control.Lens              (Ixed (ix), makeLenses, preview,
                                            view, (%%~), (%~), (&), (.~), (<&>),
                                            (?~), (^.), (^?))
import           Control.Monad.State       (MonadTrans (lift), StateT (StateT),
                                            gets)
import           Data.Array                (Array, assocs, bounds, listArray,
                                            (!), (//))
import           Data.Bifunctor            (Bifunctor (second))
import           Data.Binary               (Binary)
import           Data.Either.Combinators   (maybeToRight)
import           Data.Foldable             (find)
import qualified Data.Map                  as M
import           Data.Maybe                (isJust, isNothing, mapMaybe)
import           GHC.Generics              (Generic)
import           Gimlight.Actor            (Actor, isPlayer)
import           Gimlight.Coord            (Coord)
import           Gimlight.Dungeon.Map.Tile (TileCollection, TileIdentifier,
                                            floorTile, wallTile)
import qualified Gimlight.Dungeon.Map.Tile as Tile
import           Gimlight.Fov              (calculateFov)
import           Gimlight.Item             (Item)
import           Linear.V2                 (V2 (V2))

data Error
    = OutOfRange
    | ActorNotFound
    | ActorAlreadyExists Actor
    | ItemNotFound
    | ItemAlreadyExists Item
    | TileIsNotWalkable
    deriving (Show)

data TileIdentifierLayer =
    TileIdentifierLayer
        { _upper :: Maybe TileIdentifier
        , _lower :: Maybe TileIdentifier
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''TileIdentifierLayer

instance Binary TileIdentifierLayer

data Cell =
    Cell
        { _tileIdentifierLayer :: TileIdentifierLayer
        , _actor               :: Maybe Actor
        , _item                :: Maybe Item
        , _explored            :: Bool
        , _visibleFromPlayer   :: Bool
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''Cell

instance Binary Cell

isWalkable :: TileCollection -> Cell -> Bool
isWalkable tc c =
    all ($ c)
        [ (/= Just False) . fmap (Tile.isWalkable . (tc M.!)) .
          view (tileIdentifierLayer . upper)
        , isNothing . view actor
        ]

isTransparent :: TileCollection -> Cell -> Bool
isTransparent tc =
    (/= Just False) . fmap (Tile.isTransparent . (tc M.!)) .
    view (tileIdentifierLayer . upper)

isTileWalkable :: TileCollection -> Cell -> Bool
isTileWalkable tc c =
    fmap (Tile.isWalkable . (tc M.!)) (c ^. tileIdentifierLayer . upper) /=
    Just False

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

removeActor :: Cell -> Either Error (Actor, Cell)
removeActor c =
    fmap (, c & actor .~ Nothing) . maybeToRight ActorNotFound $ c ^. actor

removeItem :: Cell -> Either Error (Item, Cell)
removeItem c =
    fmap (, c & item .~ Nothing) . maybeToRight ItemNotFound $ c ^. item

type CellMap = Array (V2 Int) Cell

cellMap :: Array (V2 Int) TileIdentifierLayer -> CellMap
cellMap = fmap (\x -> Cell x Nothing Nothing False False)

allWallTiles :: V2 Int -> CellMap
allWallTiles wh = listArray (V2 0 0, wh - V2 1 1) $ repeat cell
  where
    cell =
        Cell
            (TileIdentifierLayer (Just wallTile) (Just floorTile))
            Nothing
            Nothing
            False
            False

widthAndHeight :: CellMap -> V2 Int
widthAndHeight = (+ V2 1 1) . snd . bounds

isPositionInMap :: Coord -> CellMap -> Bool
isPositionInMap (V2 x y) cm = x >= 0 && x < w && y >= 0 && y < h
  where
    V2 w h = widthAndHeight cm

changeTileAt ::
       (TileIdentifierLayer -> TileIdentifierLayer)
    -> Coord
    -> CellMap
    -> Maybe CellMap
changeTileAt f c m
    | isJust $ tileIdentifierLayerAt c m = Just $ m // [(c, newTile)]
    | otherwise = Nothing
  where
    newTile = m ! c & tileIdentifierLayer %~ f

walkableFloors :: TileCollection -> CellMap -> Array (V2 Int) Bool
walkableFloors tc = fmap (isWalkable tc)

exploredMap :: CellMap -> Array (V2 Int) Bool
exploredMap = fmap (view explored)

playerFov :: CellMap -> Array (V2 Int) Bool
playerFov = fmap (view visibleFromPlayer)

playerActor :: CellMap -> Maybe (Coord, Actor)
playerActor = find (isPlayer . snd) . positionsAndActors

transparentMap :: TileCollection -> CellMap -> Array (V2 Int) Bool
transparentMap tc = fmap (isTransparent tc)

updateExploredMap :: CellMap -> CellMap
updateExploredMap cm =
    cm // [(pos, cm ! pos & explored .~ True) | pos <- visibleList]
  where
    visibleList = map fst $ filter snd $ assocs $ playerFov cm

updatePlayerFov :: TileCollection -> CellMap -> Maybe CellMap
updatePlayerFov tc cm = fmap markAsVisible visibilityList
  where
    markAsVisible xs =
        cm //
        [ (pos, cm ! pos & visibleFromPlayer .~ isVisible)
        | (pos, isVisible) <- xs
        ]
    visibilityList = fmap assocs fov
    fov = (`calculateFov` transparentMap tc cm) <$> playerPosition
    playerPosition = fst <$> playerActor cm

positionsAndActors :: CellMap -> [(Coord, Actor)]
positionsAndActors = mapMaybe mapStep . assocs
  where
    mapStep (coord, cell) = (coord, ) <$> cell ^. actor

positionsAndItems :: CellMap -> [(Coord, Item)]
positionsAndItems = mapMaybe mapStep . assocs
  where
    mapStep (coord, cell) = (coord, ) <$> cell ^. item

locateActorAt ::
       TileCollection -> Actor -> Coord -> StateT CellMap (Either Error) ()
locateActorAt tc a c =
    StateT $ \cm -> fmap ((), ) $ cm & ix c %%~ locateActor tc a

locateItemAt ::
       TileCollection -> Item -> Coord -> StateT CellMap (Either Error) ()
locateItemAt tc i c =
    StateT $ \cm -> fmap ((), ) $ cm & ix c %%~ locateItem tc i

removeActorAt :: Coord -> StateT CellMap (Either Error) Actor
removeActorAt c =
    StateT $ \cm ->
        maybeToRight OutOfRange (cm ^? ix c) >>= removeActor <&>
        second (\cell -> cm // [(c, cell)])

removeItemAt :: Coord -> StateT CellMap (Either Error) Item
removeItemAt c =
    StateT $ \cm ->
        maybeToRight OutOfRange (cm ^? ix c) >>= removeItem <&>
        second (\cell -> cm // [(c, cell)])

removeActorIf :: (Actor -> Bool) -> StateT CellMap (Either Error) Actor
removeActorIf f =
    gets (fmap fst . find (f . snd) . positionsAndActors) >>= lift .
    maybeToRight ActorNotFound >>=
    removeActorAt

tileIdentifierLayerAt :: Coord -> CellMap -> Maybe TileIdentifierLayer
tileIdentifierLayerAt c = preview (ix c . tileIdentifierLayer)
