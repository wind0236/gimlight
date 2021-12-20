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
    , getPlayerActor
    , playerPosition
    , stairsPositionCandidates
    , isTown
    , getPositionsAndActors
    , positionOnParentMap
    , cellMap
    , descendingStairs
    , addAscendingAndDescendingStiars
    , addDescendingStairs
    , ascendingStairs
    ) where

import           Actor              (Actor, isPlayer)
import           Control.Lens       (makeLenses, (^.))
import           Coord              (Coord)
import           Data.Array.Base    (assocs)
import           Data.Binary        (Binary)
import           Data.Foldable      (find)
import           Dungeon.Identifier (Identifier)
import qualified Dungeon.Identifier as Identifier
import           Dungeon.Map.Cell   (CellMap, positionsAndActors,
                                     walkableFloors)
import           Dungeon.Map.Tile   (TileCollection)
import           Dungeon.Stairs     (StairsPair (StairsPair, downStairs, upStairs))
import           GHC.Generics       (Generic)

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

playerPosition :: Dungeon -> Maybe Coord
playerPosition d = fst <$> getPlayerActor d

getPlayerActor :: Dungeon -> Maybe (Coord, Actor)
getPlayerActor = find (isPlayer . snd) . positionsAndActors . (^. cellMap)

getPositionsAndActors :: Dungeon -> [(Coord, Actor)]
getPositionsAndActors = positionsAndActors . (^. cellMap)

stairsPositionCandidates :: TileCollection -> Dungeon -> [Coord]
stairsPositionCandidates ts d =
    filter (not . isStairsOnPosition) $ walkableCoords (d ^. cellMap)
  where
    walkableCoords = map fst . filter snd . assocs . walkableFloors ts
    isStairsOnPosition c = isUpStairsPosition c || isDownStairsPosition c
    isUpStairsPosition c = (downStairs <$> d ^. ascendingStairs) == Just c
    isDownStairsPosition c = c `elem` map upStairs (d ^. descendingStairs)

isTown :: Dungeon -> Bool
isTown d = Identifier.isTown $ d ^. identifier
