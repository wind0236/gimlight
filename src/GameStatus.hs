{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module GameStatus
    ( GameStatus(..)
    , newGameStatus
    ) where

import           Data.Binary                  (Binary)
import           Data.Maybe                   (fromMaybe)
import           Data.Tree                    (Tree (Node, rootLabel, subForest))
import           Dungeon                      (addAscendingAndDescendingStiars,
                                               addDescendingStairs)
import           Dungeon.Init                 (initDungeon)
import           Dungeon.Map.Tile.JSONReader  (readTileFile)
import           Dungeon.Predefined.BatsCave  (batsDungeon)
import           Dungeon.Predefined.GlobalMap (globalMap)
import           Dungeon.Stairs               (StairsPair (StairsPair))
import           GHC.Generics                 (Generic)
import           GameStatus.Exploring         (ExploringHandler,
                                               exploringHandler)
import           GameStatus.ReadingBook       (ReadingBookHandler)
import           GameStatus.Scene             (SceneHandler, sceneHandler,
                                               withoutSpeaker)
import           GameStatus.SelectingItem     (SelectingItemHandler)
import           GameStatus.Talking           (TalkingHandler)
import           Linear.V2                    (V2 (V2))
import qualified Localization.Texts           as T
import qualified Log                          as L
import           Quest                        (questCollection)
import           System.Random                (getStdGen)
import           TreeZipper                   (appendTree, goDownBy, treeZipper)

data GameStatus
    = Exploring ExploringHandler
    | Talking TalkingHandler
    | Scene SceneHandler
    | SelectingItem SelectingItemHandler
    | ReadingBook ReadingBookHandler
    | Title
    | GameOver
    | SelectingLocale
    deriving (Show, Ord, Eq, Generic)

instance Binary GameStatus

newGameStatus :: IO GameStatus
newGameStatus = do
    g <- getStdGen
    tileCollection <-
        fromMaybe (error "Failed to read the tile file.") <$>
        readTileFile "maps/tiles.json"
    gm <- globalMap
    beaeve <- initDungeon tileCollection
    let (stairsPosition, bats) = batsDungeon g tileCollection
        (gmWithBatsStairs, batsRootMapWithParentMap) =
            addAscendingAndDescendingStiars
                (StairsPair (V2 9 6) stairsPosition)
                (gm, rootLabel bats)
        batsTreeWithParentMap = bats {rootLabel = batsRootMapWithParentMap}
        (initGm, beaeveWithParentMap) =
            addDescendingStairs
                (StairsPair (V2 3 16) (V2 5 5))
                (gmWithBatsStairs, beaeve)
        dungeonTree =
            Node
                { rootLabel = initGm
                , subForest =
                      [Node {rootLabel = beaeveWithParentMap, subForest = []}]
                }
        zipper = appendTree batsTreeWithParentMap $ treeZipper dungeonTree
        initZipper =
            fromMaybe
                (error "Unreachable.")
                (goDownBy (== beaeveWithParentMap) zipper)
        initExploring =
            exploringHandler
                initZipper
                (foldr (L.addMessage . L.message) L.emptyLog [T.welcome])
                questCollection
                tileCollection
    return $
        Scene $
        sceneHandler
            "images/game_opening.png"
            [withoutSpeaker T.title1, withoutSpeaker T.title2]
            initExploring
