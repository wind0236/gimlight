{-# LANGUAGE OverloadedStrings #-}

module SetUp.Dungeon
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
import           Codec.Picture       (PixelRGBA8 (PixelRGBA8), generateImage)
import           Control.Lens        ((%~))
import           Control.Monad.State (State, evalState, execStateT)
import           Coord               (Coord)
import           Data.Array          (array, (//))
import           Data.Either         (fromRight)
import           Data.Map            (fromList)
import           Data.Maybe          (fromJust)
import           Dungeon.Map.Cell    (CellMap,
                                      TileIdentifierLayer (TileIdentifierLayer),
                                      cellMap, locateActorAt, locateItemAt)
import           Dungeon.Map.Tile    (TileCollection, tile)
import           IndexGenerator      (IndexGenerator, generator)
import           Inventory           (addItem)
import           Item                (herb, sampleBook)
import           Linear.V2           (V2 (V2))
import           UI.Draw.Config      (tileHeight, tileWidth)

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
    (p, w, i, s, orcWithoutItems, orcWithFullItems, orcWithHerb) =
        flip evalState generator $ do
            (,,,,,,) <$>
                ((inventoryItems %~ fromJust . addItem sampleBook) <$> player) <*>
                weakestOrc <*>
                intermediateOrc <*>
                strongestOrc <*>
                orc <*>
                ((!! 5) . iterate (inventoryItems %~ (fromJust . addItem herb)) <$>
                 orc) <*>
                ((inventoryItems %~ fromJust . addItem herb) <$> orc)
    emptyTile = TileIdentifierLayer Nothing Nothing
    unwalkable = TileIdentifierLayer (Just (dummyTileFile, 1)) Nothing
    mapWidth = 3
    mapHeight = 4

initTileCollection :: TileCollection
initTileCollection =
    fromList
        [ ((dummyTileFile, 0), tile True True emptyImage)
        , ((dummyTileFile, 1), tile False True emptyImage)
        ]
  where
    emptyImage = generateImage (\_ _ -> PixelRGBA8 0 0 0 0) tileWidth tileHeight

strongestOrc :: State IndexGenerator Actor
strongestOrc = monster Orc (status (hp 100) 100 100) ""

intermediateOrc :: State IndexGenerator Actor
intermediateOrc = monster Orc (status (hp 100) 50 50) ""

weakestOrc :: State IndexGenerator Actor
weakestOrc = monster Orc (status (hp 1) 0 0) ""

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

dummyTileFile :: FilePath
dummyTileFile = "dummy.json"
