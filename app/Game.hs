{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game where

import           Brick                          (AttrName)
import           Control.Lens                   (makeLenses, (%~), (&), (.=),
                                                 (.~), (^.))
import           Control.Monad.Trans.Maybe      (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans.State.Lazy (execState, modify)
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
import           Log                            (MessageLog, addMessage)
import qualified Log                            as L
import           System.Random.Stateful         (newStdGen)

data Game = Game
          { _dungeon    :: Dungeon
          , _messageLog :: MessageLog
          } deriving (Show)
makeLenses ''Game

completeThisTurn :: Game -> Game
completeThisTurn g = g { _dungeon = d', _messageLog = newLog }
    where (ms, d') = D.completeThisTurn $ g ^. dungeon
          newLog = foldl  (flip addMessage) (g ^. messageLog) ms

playerBumpAction :: Direction -> Game -> Game
playerBumpAction d g@Game{ _messageLog = log } = Game{ _dungeon = newDungeon, _messageLog = addMaybeMessage message }
    where (e, dungeon') = D.popPlayer $ g ^. dungeon
          (newDungeon, message) = D.bumpAction e (directionToOffset d) dungeon'
          addMaybeMessage (Just m) = addMessage m log
          addMaybeMessage Nothing  = log

initGame :: IO Game
initGame = do
        dungeon <- initDungeon
        return $ Game { _dungeon = dungeon
                      , _messageLog = foldr (addMessage . L.infoMessage) L.emptyLog ["Welcome to a roguelike game!"]
                      }
