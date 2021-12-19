{-# LANGUAGE OverloadedStrings #-}

module SetUp
    ( initCellMap
    , initTileCollection
    , playerPosition
    , orcWithoutItemsPosition
    , orcWithFullItemsPosition
    , strongestOrcPosition
    , intermediateOrcPosition
    , weakestOrcPosition
    , orcWithHerbPosition
    ) where

import           Actor               (Actor, inventoryItems, monster, player)
import           Actor.Identifier    (Identifier (Orc))
import           Actor.Monsters      (orc)
import           Actor.Status        (status)
import           Actor.Status.Hp     (hp)
import           Control.Lens        ((%~))
import           Control.Monad.State (execStateT)
import           Coord               (Coord)
import           Data.Array          (array, (//))
import           Data.Bifunctor      (Bifunctor (first))
import           Data.Either         (fromRight)
import           Data.Maybe          (fromJust)
import           Dungeon.Map.Cell    (CellMap, TileIdLayer (TileIdLayer),
                                      cellMap, locateActorAt, locateItemAt)
import           Dungeon.Map.Tile    (TileCollection, tile)
import           IndexGenerator      (IndexGenerator, generator)
import           Inventory           (addItem)
import           Item                (herb, sampleBook)
import           Linear.V2           (V2 (V2))

initCellMap :: CellMap
initCellMap =
    fromRight (error "Failed to set up the test environment.") $
    flip execStateT emptyMap $ do
        locateActorAt initTileCollection p playerPosition
        locateItemAt initTileCollection herb playerPosition
        locateItemAt initTileCollection herb orcWithFullItemsPosition
        locateActorAt initTileCollection orcWithoutItems orcWithoutItemsPosition
        locateActorAt
            initTileCollection
            orcWithFullItems
            orcWithFullItemsPosition
        locateActorAt initTileCollection s strongestOrcPosition
        locateActorAt initTileCollection i intermediateOrcPosition
        locateActorAt initTileCollection w weakestOrcPosition
        locateActorAt initTileCollection orcWithHerb orcWithHerbPosition
  where
    emptyMap =
        cellMap $
        array
            (V2 0 0, V2 (mapWidth - 1) (mapHeight - 1))
            [ (V2 x y, emptyTile)
            | x <- [0 .. mapWidth - 1]
            , y <- [0 .. mapHeight - 1]
            ] //
        [(V2 0 1, unwalkable)]
    (p, g) =
        first (inventoryItems %~ (fromJust . addItem sampleBook)) $
        player generator
    (w, g') = weakestOrc g
    (i, g'') = intermediateOrc g'
    (s, g''') = strongestOrc g''
    (orcWithoutItems, g'''') = orc g'''
    (orcWithFullItems, g''''') =
        iterate
            (first (inventoryItems %~ (fromJust . addItem herb)))
            (orc g'''') !!
        5
    (orcWithHerb, _) =
        first (inventoryItems %~ (fromJust . addItem herb)) $ orc g'''''
    emptyTile = TileIdLayer Nothing Nothing
    unwalkable = TileIdLayer (Just 1) Nothing
    mapWidth = 3
    mapHeight = 4

initTileCollection :: TileCollection
initTileCollection = array (0, 1) [(0, tile True True), (1, tile False True)]

strongestOrc :: IndexGenerator -> (Actor, IndexGenerator)
strongestOrc g = monster g Orc (status (hp 100) 100 100) ""

intermediateOrc :: IndexGenerator -> (Actor, IndexGenerator)
intermediateOrc g = monster g Orc (status (hp 100) 50 50) ""

weakestOrc :: IndexGenerator -> (Actor, IndexGenerator)
weakestOrc g = monster g Orc (status (hp 1) 0 0) ""

playerPosition :: Coord
playerPosition = V2 0 0

orcWithoutItemsPosition :: Coord
orcWithoutItemsPosition = V2 1 0

orcWithFullItemsPosition :: Coord
orcWithFullItemsPosition = V2 2 0

strongestOrcPosition :: Coord
strongestOrcPosition = V2 1 2

intermediateOrcPosition :: Coord
intermediateOrcPosition = V2 0 3

weakestOrcPosition :: Coord
weakestOrcPosition = V2 1 3

orcWithHerbPosition :: Coord
orcWithHerbPosition = V2 2 1
