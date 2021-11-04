{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module GameModel.Status
    ( GameStatus(..)
    , newGameStatus
    ) where

import           Data.Binary                         (Binary)
import           Data.Tree                           (Tree (Node, rootLabel, subForest))
import           Dungeon                             (addAscendingAndDescendingStiars,
                                                      addDescendingStairs)
import           Dungeon.Init                        (initDungeon)
import           Dungeon.Predefined.BatsCave         (batsDungeon)
import           Dungeon.Predefined.GlobalMap        (globalMap)
import           Dungeon.Stairs                      (StairsPair (StairsPair))
import           GHC.Generics                        (Generic)
import           GameModel.Status.Exploring          (ExploringHandler,
                                                      exploringHandler)
import           GameModel.Status.ReadingBook        (ReadingBookHandler)
import           GameModel.Status.Scene              (SceneHandler,
                                                      sceneHandler)
import           GameModel.Status.SelectingItemToUse (SelectingItemToUseHandler)
import           GameModel.Status.Talking            (TalkingHandler)
import           Linear.V2                           (V2 (V2))
import qualified Localization.Texts                  as T
import qualified Log                                 as L
import           Scene                               (gameStartScene)
import           System.Random                       (getStdGen)
import           TreeZipper                          (appendTree, goDownBy,
                                                      treeZipper)

data GameStatus
    = Exploring ExploringHandler
    | Talking TalkingHandler
    | Scene SceneHandler
    | SelectingItemToUse SelectingItemToUseHandler
    | ReadingBook ReadingBookHandler
    | Title
    | GameOver
    | SelectingLocale
    deriving (Show, Ord, Eq, Generic)

instance Binary GameStatus

newGameStatus :: IO GameStatus
newGameStatus = do
    g <- getStdGen
    let (stairsPosition, bats) = batsDungeon g
        beaeve = initDungeon
        gm = globalMap
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
            case goDownBy (== beaeveWithParentMap) zipper of
                Just x  -> x
                Nothing -> error "Unreachable."
        initExploring =
            exploringHandler initZipper $
            foldr (L.addMessage . L.message) L.emptyLog [T.welcome]
    return $ Scene $ sceneHandler gameStartScene initExploring
