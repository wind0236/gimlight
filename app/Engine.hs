{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Engine where

import           Brick                          (AttrName)
import           Control.Lens                   (makeLenses, makeLensesFor, use,
                                                 (%=), (%~), (&), (.=), (.~),
                                                 (^.), (^?!))
import           Control.Monad.Extra            (foldM)
import           Control.Monad.Trans.Maybe      (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans.State      (State, evalState, get, state)
import           Control.Monad.Trans.State.Lazy (execState, modify, runState)
import           Coord                          (Coord)
import           Data.Array                     (Array)
import           Data.Array.Base                (array, bounds, elems, (!),
                                                 (//))
import           Data.Maybe                     (catMaybes)
import           Dungeon                        (Dungeon, aliveEnemies, enemies,
                                                 getPlayerEntity, initDungeon)
import qualified Dungeon                        as D
import           Dungeon.Generate               (generateDungeon)
import           Dungeon.Size                   (height, maxRooms, roomMaxSize,
                                                 roomMinSize, width)
import qualified Dungeon.Turn                   as DT
import           Dungeon.Types                  (maxHp, position)
import           Entity                         (Entity (..))
import qualified Entity                         as E
import           Entity.Behavior                (bumpAction, enemyAction)
import           Event                          (Event, gameStartEvent)
import           Graphics.Vty.Attributes.Color  (Color, white, yellow)
import           Linear.V2                      (V2 (..), _x, _y)
import           Log                            (MessageLog, addMaybeMessage,
                                                 addMessage, addMessages)
import qualified Log                            as L
import           Map.Bool                       (BoolMap, emptyBoolMap)
import           Map.Tile                       (Tile, TileMap, darkAttr,
                                                 lightAttr, transparent,
                                                 walkable)
import           System.Random.Stateful         (newStdGen)

data Engine = Engine
          { _dungeon    :: Dungeon
          , _messageLog :: MessageLog
          , _isGameOver :: Bool
          } | HandlingEvent
          { _event       :: Event
          , _afterFinish :: Engine
          } deriving (Show)
makeLenses ''Engine

completeThisTurn :: State Engine ()
completeThisTurn = do
        handleEnemyTurns

        e <- get
        let dg = e ^?! dungeon

        let (status, newD) = runState D.completeThisTurn dg

        isGameOver .= (status == DT.PlayerKilled)

        dungeon .= newD

handleEnemyTurns :: State Engine ()
handleEnemyTurns = do
        e <- get
        let dg = e ^?! dungeon

        let xs = aliveEnemies dg

        mapM_ (handleEnemyTurn . (^. position)) xs

handleEnemyTurn :: Coord -> State Engine ()
handleEnemyTurn c = do
        e <- get
        let dg = e ^?! dungeon

        let (messages, dg') = flip runState dg $ do
                e <- D.popActorAt c
                case e of
                    Just e  -> enemyAction e
                    Nothing -> error "No such enemy."

        messageLog %= addMessages messages
        dungeon .= dg'

playerBumpAction :: V2 Int -> State Engine ()
playerBumpAction offset = do
        e <- get
        let dg = e ^?! dungeon

        let (messages, newDungeon) = flip runState dg $ do
                e <- D.popPlayer
                bumpAction e offset

        messageLog %= addMessages messages
        dungeon .= newDungeon

playerCurrentHp :: Engine -> Int
playerCurrentHp e = E.getHp $ getPlayerEntity (e ^?! dungeon)

playerMaxHp :: Engine -> Int
playerMaxHp e = getPlayerEntity (e ^?! dungeon) ^. maxHp

initEngine :: IO Engine
initEngine = do
        dungeon <- initDungeon
        return $ HandlingEvent {
                               _event = gameStartEvent,
                               _afterFinish = Engine { _dungeon = dungeon
                      , _messageLog = foldr (addMessage . L.message) L.emptyLog ["Welcome to a roguelike game!"]
                      , _isGameOver = False
                      }
                      }
