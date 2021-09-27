{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Engine where

import           Actions                        (bumpAction)
import           Brick                          (AttrName)
import           Control.Lens                   (makeLenses, (%~), (&), (.=),
                                                 (.~), (^.))
import           Control.Monad.Trans.Maybe      (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans.State      (State, state)
import           Control.Monad.Trans.State.Lazy (execState, modify, runState)
import           Coord                          (Coord)
import           Data.Array                     (Array)
import           Data.Array.Base                (array, bounds, elems, (!),
                                                 (//))
import           Direction                      (Direction (East, North, South, West),
                                                 directionToOffset)
import           Dungeon                        (Dungeon, initDungeon)
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
                                                 addMessage)
import qualified Log                            as L
import           System.Random.Stateful         (newStdGen)

data Engine = Engine
          { _dungeon    :: Dungeon
          , _messageLog :: MessageLog
          } deriving (Show)
makeLenses ''Engine

completeThisTurn :: Engine -> Engine
completeThisTurn g = g { _dungeon = d', _messageLog = newLog }
    where (ms, d') = runState D.completeThisTurn $ g ^. dungeon
          newLog = foldl  (flip addMessage) (g ^. messageLog) ms

playerBumpAction :: Direction -> Engine -> Engine
playerBumpAction d g@Engine{ _messageLog = log } = Engine{ _dungeon = newDungeon, _messageLog = addMaybeMessage message log }
    where (message, newDungeon) = flip runState (g ^. dungeon) $ do
            e <- D.popPlayer
            bumpAction e (directionToOffset d)

initEngine :: IO Engine
initEngine = do
        dungeon <- initDungeon
        return $ Engine { _dungeon = dungeon
                      , _messageLog = foldr (addMessage . L.infoMessage) L.emptyLog ["Welcome to a roguelike game!"]
                      }
