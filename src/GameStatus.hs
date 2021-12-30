{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module GameStatus
    ( GameStatus(..)
    , newGameStatus
    ) where

import           Control.Lens                 ()
import           Data.Binary                  (Binary)
import           Data.Map                     (empty)
import           Data.Maybe                   (fromMaybe)
import           Data.Tree                    (Tree (Node, rootLabel, subForest))
import           Dungeon                      (addAscendingAndDescendingStiars,
                                               addDescendingStairs)
import           Dungeon.Init                 (initDungeon)
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
import           IndexGenerator               (generator)
import           Linear.V2                    (V2 (V2))
import qualified Localization.Texts.Scene     as T (title1, title2, welcome)
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
    (gm, tc) <- globalMap empty
    (beaeve, tc', ig) <- initDungeon tc generator
    let (stairsPosition, bats, _, _) = batsDungeon g ig tc'
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
                tc'
    return . Scene $
        sceneHandler
            "images/game_opening.png"
            [withoutSpeaker T.title1, withoutSpeaker T.title2]
            initExploring
