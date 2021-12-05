-- I refer to "Dungeon" in this source code as the mixed things of map and
-- actors because I could not come up with a much more proper word.  So,
-- in this code, "Dungeon" means not only dungeon but also towns, etc.
--
-- TODO: Change the word to more precise one.
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Dungeon
    ( Dungeon
    , dungeon
    , getIdentifier
    , popActorAt
    , pushActor
    , walkableFloor
    , getPlayerActor
    , mapWidthAndHeight
    , playerPosition
    , stairsPositionCandidates
    , updateMap
    , calculateFovAt
    , isTown
    , isPositionInDungeon
    , positionsAndNpcs
    , getPositionsAndActors
    , positionOnParentMap
    , cellMap
    , descendingStairs
    , addAscendingAndDescendingStiars
    , addDescendingStairs
    , ascendingStairs
    ) where

import           Actor              (Actor, isPlayer)
import           Control.Lens       (makeLenses, (%~), (&), (.~), (^.))
import           Coord              (Coord)
import           Data.Array         (Array)
import           Data.Array.Base    (assocs)
import           Data.Binary        (Binary)
import           Data.Foldable      (find)
import           Dungeon.Identifier (Identifier)
import qualified Dungeon.Identifier as Identifier
import           Dungeon.Map.Cell   (CellMap, locateActorAt, positionsAndActors,
                                     removeActorAt, updateExploredMap,
                                     updatePlayerFov, walkableMap,
                                     widthAndHeight)
import qualified Dungeon.Map.Cell   as Cell
import           Dungeon.Map.Tile   (TileCollection)
import           Dungeon.Stairs     (StairsPair (StairsPair, downStairs, upStairs))
import           Fov                (Fov, calculateFov)
import           GHC.Generics       (Generic)
import           Linear.V2          (V2 (..))

data Dungeon =
    Dungeon
        { _cellMap             :: CellMap
        , _positionOnParentMap :: Maybe Coord
          -- Do not integrate `_ascendingStairs` with
          -- `_positionOnParentMap` For example, towns have a `Just`
          -- `_positionOnParentMap` but they do not have ascending stairs
          -- to the global map.
        , _ascendingStairs     :: Maybe StairsPair
        , _descendingStairs    :: [StairsPair]
        , _identifier          :: Identifier
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''Dungeon

instance Binary Dungeon

dungeon :: CellMap -> Identifier -> Dungeon
dungeon c ident =
    Dungeon
        { _cellMap = c
        , _positionOnParentMap = Nothing
        , _ascendingStairs = Nothing
        , _descendingStairs = []
        , _identifier = ident
        }

getIdentifier :: Dungeon -> Identifier
getIdentifier d = d ^. identifier

addAscendingAndDescendingStiars ::
       StairsPair -> (Dungeon, Dungeon) -> (Dungeon, Dungeon)
addAscendingAndDescendingStiars sp@(StairsPair upper _) (parent@Dungeon {_descendingStairs = ss}, child@Dungeon { _ascendingStairs = Nothing
                                                                                                                , _positionOnParentMap = Nothing
                                                                                                                }) =
    ( parent {_descendingStairs = sp : ss}
    , child {_ascendingStairs = Just sp, _positionOnParentMap = Just upper})
addAscendingAndDescendingStiars _ _ =
    error "The child's position and the ascending stairs are already set."

addDescendingStairs :: StairsPair -> (Dungeon, Dungeon) -> (Dungeon, Dungeon)
addDescendingStairs sp@(StairsPair upper _) (parent@Dungeon {_descendingStairs = ss}, child@Dungeon {_positionOnParentMap = Nothing}) =
    ( parent {_descendingStairs = sp : ss}
    , child {_positionOnParentMap = Just upper})
addDescendingStairs _ _ =
    error "The child's position in the parent map is already set."

updateMap :: TileCollection -> Dungeon -> Maybe Dungeon
updateMap ts = updateFov ts . updateExplored

updateExplored :: Dungeon -> Dungeon
updateExplored d = d & cellMap %~ updateExploredMap

updateFov :: TileCollection -> Dungeon -> Maybe Dungeon
updateFov ts d =
    (\newMap -> d & cellMap .~ newMap) <$> updatePlayerFov ts (d ^. cellMap)

calculateFovAt :: Coord -> TileCollection -> Dungeon -> Fov
calculateFovAt c ts d = calculateFov c (transparentMap ts d)

playerPosition :: Dungeon -> Maybe Coord
playerPosition d = fst <$> getPlayerActor d

getPlayerActor :: Dungeon -> Maybe (Coord, Actor)
getPlayerActor = find (isPlayer . snd) . positionsAndActors . (^. cellMap)

getPositionsAndActors :: Dungeon -> [(Coord, Actor)]
getPositionsAndActors = positionsAndActors . (^. cellMap)

pushActor :: Coord -> Actor -> Dungeon -> Dungeon
pushActor p e d =
    case locateActorAt e p (d ^. cellMap) of
        Just x  -> d & cellMap .~ x
        Nothing -> error "Failed to push an actor."

popActorAt :: Coord -> Dungeon -> (Maybe Actor, Dungeon)
popActorAt c d =
    case removeActorAt c (d ^. cellMap) of
        Just (a, newMap) -> (Just a, d & cellMap .~ newMap)
        Nothing          -> (Nothing, d)

stairsPositionCandidates :: TileCollection -> Dungeon -> [Coord]
stairsPositionCandidates ts d =
    filter (not . isStairsOnPosition) $ walkableCoords d
  where
    walkableCoords = map fst . filter snd . assocs . walkableFloor ts
    isStairsOnPosition c = isUpStairsPosition c || isDownStairsPosition c
    isUpStairsPosition c = (downStairs <$> d ^. ascendingStairs) == Just c
    isDownStairsPosition c = c `elem` map upStairs (d ^. descendingStairs)

walkableFloor :: TileCollection -> Dungeon -> Array (V2 Int) Bool
walkableFloor ts d = walkableMap ts (d ^. cellMap)

transparentMap :: TileCollection -> Dungeon -> Array (V2 Int) Bool
transparentMap ts d = Cell.transparentMap ts (d ^. cellMap)

positionsAndNpcs :: Dungeon -> [(Coord, Actor)]
positionsAndNpcs =
    filter (not . isPlayer . snd) . positionsAndActors . (^. cellMap)

mapWidthAndHeight :: Dungeon -> V2 Int
mapWidthAndHeight d = widthAndHeight (d ^. cellMap)

isTown :: Dungeon -> Bool
isTown d = Identifier.isTown $ d ^. identifier

isPositionInDungeon :: Coord -> Dungeon -> Bool
isPositionInDungeon c d = x >= 0 && x < width && y >= 0 && y < height
  where
    V2 width height = mapWidthAndHeight d
    V2 x y = c
