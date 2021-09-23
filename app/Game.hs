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
import           Direction                      (Direction (East, North, South, West))
import           Dungeon                        (Dungeon, initDungeon)
import qualified Dungeon                        as D
import           Dungeon.BoolMap                (BoolMap, emptyBoolMap)
import           Dungeon.GameMap                (GameMap)
import           Dungeon.Generate               (generateDungeon)
import           Dungeon.Size                   (height, maxRooms, roomMaxSize,
                                                 roomMinSize, width)
import           Dungeon.Tile                   (Tile, darkAttr, lightAttr,
                                                 transparent, walkable)
import           Entity                         (Entity (..), playerEntity,
                                                 position)
import           Graphics.Vty.Attributes.Color  (Color, white, yellow)
import           Linear.V2                      (V2 (..), _x, _y)
import           Message                        (MessageLog, addMessage)
import qualified Message                        as M
import           System.Random.Stateful         (newStdGen)

data Game = Game
          { _dungeon    :: Dungeon
          , _messageLog :: MessageLog
          } deriving (Show)
makeLenses ''Game

updateMap :: Game -> Game
updateMap g = g & dungeon %~ D.updateMap

bumpAction :: Direction -> Game -> Game
bumpAction d g@Game{ _messageLog = log } = Game{ _dungeon = newDungeon, _messageLog = addMaybeMessage message }
    where (newDungeon, message) = D.bumpAction d (g ^. dungeon)
          addMaybeMessage (Just m) = addMessage m log
          addMaybeMessage Nothing  = log

initGame :: IO Game
initGame = do
        dungeon <- initDungeon
        return $ Game { _dungeon = dungeon
                      , _messageLog = foldr (addMessage . M.infoMessage) M.emptyLog ["Welcome to a roguelike game!"]
                      }
