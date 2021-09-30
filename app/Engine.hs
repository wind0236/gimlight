{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

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
import           Dungeon                        (Dungeon, aliveEnemies, enemies,
                                                 getPlayerEntity, initDungeon)
import qualified Dungeon                        as D
import           Dungeon.Generate               (generateDungeon)
import           Dungeon.Map.Bool               (BoolMap, emptyBoolMap)
import           Dungeon.Map.Tile               (Tile, TileMap, darkAttr,
                                                 lightAttr, transparent,
                                                 walkable)
import           Dungeon.Size                   (height, maxRooms, roomMaxSize,
                                                 roomMinSize, width)
import qualified Dungeon.Turn                   as DT
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
          , _isGameOver :: Bool
          } deriving (Show)
makeLenses ''Engine

completeThisTurn :: State Engine ()
completeThisTurn = do
        handleEnemyTurns

        dg <- use dungeon

        let (status, newD) = runState D.completeThisTurn dg

        isGameOver .= (status == DT.PlayerKilled)

        dungeon .= newD

handleEnemyTurns :: State Engine ()
handleEnemyTurns = do
        dg <- use dungeon

        let xs = aliveEnemies dg

        mapM_ (handleEnemyTurn . (^. position)) xs

handleEnemyTurn :: Coord -> State Engine ()
handleEnemyTurn c = do
        dg <- use dungeon

        let (messages, dg') = flip runState dg $ do
                e <- D.popActorAt c
                case e of
                    Just e  -> enemyAction e
                    Nothing -> error "No such enemy."

        messageLog %= addMessages messages
        dungeon .= dg'

playerBumpAction :: V2 Int -> State Engine ()
playerBumpAction offset = do
        dg <- use dungeon

        let (messages, newDungeon) = flip runState dg $ do
                e <- D.popPlayer
                bumpAction e offset

        messageLog %= addMessages messages
        dungeon .= newDungeon

playerCurrentHp :: Engine -> Int
playerCurrentHp e = E.getHp $ getPlayerEntity (e ^. dungeon)

playerMaxHp :: Engine -> Int
playerMaxHp e = getPlayerEntity (e ^. dungeon) ^. E.maxHp

initEngine :: IO Engine
initEngine = do
        dungeon <- initDungeon
        return $ Engine { _dungeon = dungeon
                      , _messageLog = foldr (addMessage . L.infoMessage) L.emptyLog ["Welcome to a roguelike game!"]
                      , _isGameOver = False
                      }
