{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Status
    ( GameStatus(..)
    , newGameStatus
    ) where

import           Data.Binary                    (Binary)
import           Data.Tree                      (Tree (Node, rootLabel, subForest))
import           Dungeon                        (addAscendingAndDescendingStiars,
                                                 addDescendingStairs)
import           Dungeon.Init                   (initDungeon)
import           Dungeon.Predefined.BatsCave    (batsDungeon)
import           Dungeon.Predefined.GlobalMap   (globalMap)
import           Dungeon.Stairs                 (StairsPair (StairsPair))
import           GHC.Generics                   (Generic)
import           Game.Status.Exploring          (ExploringHandler,
                                                 exploringHandler)
import           Game.Status.Scene              (SceneHandler, sceneHandler)
import           Game.Status.SelectingItemToUse (SelectingItemToUseHandler)
import           Game.Status.Talking            (TalkingHandler)
import           Linear.V2                      (V2 (V2))
import           Localization                   (multilingualText)
import qualified Log                            as L
import           Scene                          (gameStartScene)
import           System.Random                  (getStdGen)
import           TreeZipper                     (appendTree, goDownBy,
                                                 treeZipper)

data GameStatus = Exploring ExploringHandler
                | Talking TalkingHandler
                | HandlingScene SceneHandler
                | SelectingItemToUse SelectingItemToUseHandler
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
            addAscendingAndDescendingStiars (StairsPair (V2 9 6) stairsPosition) (gm, rootLabel bats)

        batsTreeWithParentMap = bats { rootLabel = batsRootMapWithParentMap }

        (initGm, beaeveWithParentMap) =
            addDescendingStairs (StairsPair (V2 3 16) (V2 5 5)) (gmWithBatsStairs, beaeve)

        dungeonTree = Node { rootLabel = initGm
                           , subForest = [ Node { rootLabel = beaeveWithParentMap
                                                , subForest = []
                                                }
                                         ]
                           }
        zipper = appendTree batsTreeWithParentMap $ treeZipper dungeonTree
        initZipper = case goDownBy (== beaeveWithParentMap) zipper of
                         Just x  -> x
                         Nothing -> error "Unreachable."

        initExploring = exploringHandler
            initZipper $
            foldr (L.addMessage . L.message) L.emptyLog
                [multilingualText "Welcome to a roguelike game!" "ローグライクゲームへようこそ！"]

    return $ HandlingScene $ sceneHandler gameStartScene initExploring
