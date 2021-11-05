{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module GameModel.Status.Exploring
    ( ExploringHandler
    , exploringHandler
    , ascendStairsAtPlayerPosition
    , descendStairsAtPlayerPosition
    , exitDungeon
    , doPlayerAction
    , completeThisTurn
    , getPlayerActor
    , getPlayerPosition
    , actorAt
    , isPositionInDungeon
    , getCurrentDungeon
    , getMessageLog
    ) where

import           Actor                               (Actor)
import           Actor.Actions                       (Action, ActionStatus)
import           Control.Lens                        (makeLenses, (%~), (&),
                                                      (.~), (^.))
import           Control.Monad.Trans.Writer          (runWriter)
import           Coord                               (Coord)
import           Data.Binary                         (Binary)
import           Dungeon                             (Dungeon)
import qualified Dungeon                             as D
import           GHC.Generics                        (Generic)
import           GameModel.Status.Exploring.Dungeons (Dungeons)
import qualified GameModel.Status.Exploring.Dungeons as DS
import           Log                                 (MessageLog)
import qualified Log                                 as L
import           TreeZipper                          (TreeZipper, getFocused,
                                                      modify)

data ExploringHandler =
    ExploringHandler
        { _dungeons   :: Dungeons
        , _messageLog :: MessageLog
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''ExploringHandler

instance Binary ExploringHandler

exploringHandler :: TreeZipper Dungeon -> MessageLog -> ExploringHandler
exploringHandler = ExploringHandler

ascendStairsAtPlayerPosition :: ExploringHandler -> Maybe ExploringHandler
ascendStairsAtPlayerPosition eh =
    (\x -> eh & dungeons .~ x) <$>
    DS.ascendStairsAtPlayerPosition (eh ^. dungeons)

descendStairsAtPlayerPosition :: ExploringHandler -> Maybe ExploringHandler
descendStairsAtPlayerPosition eh =
    (\x -> eh & dungeons .~ x) <$>
    DS.descendStairsAtPlayerPosition (eh ^. dungeons)

exitDungeon :: ExploringHandler -> Maybe ExploringHandler
exitDungeon eh = (\x -> eh & dungeons .~ x) <$> DS.exitDungeon (eh ^. dungeons)

doPlayerAction :: Action -> ExploringHandler -> (ActionStatus, ExploringHandler)
doPlayerAction action eh = (status, newHandler)
  where
    ((status, dungeonsAfterAction), newLog) =
        runWriter $ DS.doPlayerAction action $ eh ^. dungeons
    newHandler =
        eh & messageLog %~ L.addMessages newLog &
        dungeons .~ dungeonsAfterAction

completeThisTurn :: ExploringHandler -> Maybe ExploringHandler
completeThisTurn eh =
    (\x -> handlerAfterNpcTurns & dungeons %~ modify (const x)) <$>
    newCurrentDungeon
  where
    handlerAfterNpcTurns = handleNpcTurns eh
    newCurrentDungeon =
        D.updateMap $ getFocused $ handlerAfterNpcTurns ^. dungeons

handleNpcTurns :: ExploringHandler -> ExploringHandler
handleNpcTurns eh =
    eh & dungeons .~ dungeonsAfterNpcTurns & messageLog %~ L.addMessages newLog
  where
    (dungeonsAfterNpcTurns, newLog) =
        runWriter $ DS.handleNpcTurns $ eh ^. dungeons

getPlayerActor :: ExploringHandler -> Maybe Actor
getPlayerActor = D.getPlayerActor . getCurrentDungeon

getPlayerPosition :: ExploringHandler -> Maybe Coord
getPlayerPosition = D.playerPosition . getCurrentDungeon

actorAt :: Coord -> ExploringHandler -> Maybe Actor
actorAt c = D.actorAt c . getCurrentDungeon

isPositionInDungeon :: Coord -> ExploringHandler -> Bool
isPositionInDungeon c = D.isPositionInDungeon c . getCurrentDungeon

getCurrentDungeon :: ExploringHandler -> Dungeon
getCurrentDungeon eh = getFocused $ eh ^. dungeons

getMessageLog :: ExploringHandler -> MessageLog
getMessageLog eh = eh ^. messageLog
