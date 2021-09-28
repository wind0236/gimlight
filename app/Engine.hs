{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Engine where

import           Actions                        (bumpAction, enemyAction)
import           Brick                          (AttrName)
import           Control.Lens                   (makeLenses, use, (%=), (%~),
                                                 (&), (.=), (.~), (^.))
import           Control.Monad.Extra            (foldM)
import           Control.Monad.Trans.Maybe      (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans.State      (State, evalState, state)
import           Control.Monad.Trans.State.Lazy (execState, modify, runState)
import           Coord                          (Coord)
import           Data.Array                     (Array)
import           Data.Array.Base                (array, bounds, elems, (!),
                                                 (//))
import           Data.Maybe                     (catMaybes)
import           Dungeon                        (Dungeon, enemies, initDungeon)
import qualified Dungeon                        as D
import           Dungeon.Generate               (generateDungeon)
import           Dungeon.Map.Bool               (BoolMap, emptyBoolMap)
import           Dungeon.Map.Tile               (Tile, TileMap, darkAttr,
                                                 lightAttr, transparent,
                                                 walkable)
import           Dungeon.Size                   (height, maxRooms, roomMaxSize,
                                                 roomMinSize, width)
import           Entity                         (Entity (..), position)
import qualified Entity                         as E
import           Graphics.Vty.Attributes.Color  (Color, white, yellow)
import           Linear.V2                      (V2 (..), _x, _y)
import           Log                            (MessageLog, addMaybeMessage,
                                                 addMessage, addMessages)
import qualified Log                            as L
import           System.Random.Stateful         (newStdGen)

data Engine = Engine
          { _dungeon    :: Dungeon
          , _messageLog :: MessageLog
          } deriving (Show)
makeLenses ''Engine

completeThisTurn :: State Engine ()
completeThisTurn = do
        handleEnemyTurns

        dg <- use dungeon

        let (ms, newD) = runState D.completeThisTurn dg

        messageLog %= addMessages ms

        dungeon .= newD

handleEnemyTurns :: State Engine ()
handleEnemyTurns = do
        dg <- use dungeon
        let xs = evalState enemies dg
        mapM_ (handleEnemyTurn . (^. position)) xs

handleEnemyTurn :: Coord -> State Engine ()
handleEnemyTurn c = do
        dg <- use dungeon

        let (message, dg') = flip runState dg $ do
                e <- D.popActorAt c
                case e of
                    Just e  -> enemyAction e
                    Nothing -> error "No such enemy."

        messageLog %= addMaybeMessage message
        dungeon .= dg'

playerBumpAction :: V2 Int -> State Engine ()
playerBumpAction offset = do
        dg <- use dungeon

        let (message, newDungeon) = flip runState dg $ do
                e <- D.popPlayer
                bumpAction e offset

        messageLog %= addMaybeMessage message
        dungeon .= newDungeon

initEngine :: IO Engine
initEngine = do
        dungeon <- initDungeon
        return $ Engine { _dungeon = dungeon
                      , _messageLog = foldr (addMessage . L.infoMessage) L.emptyLog ["Welcome to a roguelike game!"]
                      }
