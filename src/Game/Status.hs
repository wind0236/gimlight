{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Status
    ( GameStatus(..)
    , newGameStatus
    ) where

import           Data.Binary                    (Binary)
import           Dungeon.Init                   (initDungeon)
import           Dungeon.Predefined.BatsCave    (batsDungeon)
import           Dungeon.Predefined.GlobalMap   (globalMap)
import           GHC.Generics                   (Generic)
import           Game.Status.Exploring          (ExploringHandler,
                                                 exploringHandler)
import           Game.Status.Scene              (SceneHandler, sceneHandler)
import           Game.Status.SelectingItemToUse (SelectingItemToUseHandler)
import           Game.Status.Talking            (TalkingHandler)
import           Localization                   (multilingualText)
import qualified Log                            as L
import           Scene                          (gameStartScene)
import           System.Random                  (getStdGen)

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

    let bats = batsDungeon g
        initExploring = exploringHandler
            initDungeon
            [globalMap, bats] $
            foldr (L.addMessage . L.message) L.emptyLog
                [multilingualText "Welcome to a roguelike game!" "ローグライクゲームへようこそ！"]

    return $ HandlingScene $ sceneHandler gameStartScene initExploring
