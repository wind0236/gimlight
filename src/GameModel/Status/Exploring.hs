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
    , addMessages
    , getCurrentDungeon
    , getMessageLog
    ) where

import           Control.Lens                        (makeLenses, (%~), (&),
                                                      (.~), (^.))
import           Control.Monad.Trans.Maybe           (MaybeT (runMaybeT))
import           Control.Monad.Trans.Writer          (runWriter)
import           Coord                               (Coord)
import           Data.Binary                         (Binary)
import           Dungeon                             (Dungeon)
import qualified Dungeon                             as D
import           Dungeon.Actor                       (Actor)
import           Dungeon.Actor.Actions               (Action)
import           Dungeon.Turn                        (Status (PlayerKilled))
import           GHC.Generics                        (Generic)
import           GameModel.Status.Exploring.Dungeons (Dungeons)
import qualified GameModel.Status.Exploring.Dungeons as DS
import           Log                                 (Message, MessageLog)
import qualified Log                                 as L
import           TreeZipper                          (TreeZipper, getFocused,
                                                      modify)

data ExploringHandler = ExploringHandler
                      { _dungeons   :: Dungeons
                      , _messageLog :: MessageLog
                      } deriving (Show, Ord, Eq, Generic)

makeLenses ''ExploringHandler

instance Binary ExploringHandler

exploringHandler :: TreeZipper Dungeon -> MessageLog -> ExploringHandler
exploringHandler = ExploringHandler

ascendStairsAtPlayerPosition :: ExploringHandler -> Maybe ExploringHandler
ascendStairsAtPlayerPosition eh = (\x -> eh & dungeons .~ x) <$> DS.ascendStairsAtPlayerPosition (eh ^. dungeons)

descendStairsAtPlayerPosition :: ExploringHandler -> Maybe ExploringHandler
descendStairsAtPlayerPosition eh = (\x -> eh & dungeons .~ x) <$> DS.descendStairsAtPlayerPosition (eh ^. dungeons)

exitDungeon :: ExploringHandler -> Maybe ExploringHandler
exitDungeon eh = (\x -> eh & dungeons .~ x) <$> DS.exitDungeon (eh ^. dungeons)

doPlayerAction :: Action -> ExploringHandler -> (Bool, ExploringHandler)
doPlayerAction action eh =
    result
    where (dungeonsAfterAction, newLog) = runWriter $ runMaybeT $ DS.doPlayerAction action $ eh ^. dungeons
          handlerWithNewLog = eh & messageLog %~ L.addMessages newLog

          result = case dungeonsAfterAction of
                       Just x  -> (True, handlerWithNewLog & dungeons .~ x)
                       Nothing -> (False, handlerWithNewLog)

completeThisTurn :: ExploringHandler -> Maybe ExploringHandler
completeThisTurn eh =
    if status == PlayerKilled
        then Nothing
        else Just $ handlerAfterNpcTurns & dungeons %~ modify (const newCurrentDungeon)
    where handlerAfterNpcTurns = handleNpcTurns eh
          (status, newCurrentDungeon) = D.completeThisTurn $ getFocused $ handlerAfterNpcTurns ^. dungeons

handleNpcTurns :: ExploringHandler -> ExploringHandler
handleNpcTurns eh =
    eh & dungeons .~ dungeonsAfterNpcTurns & messageLog %~ L.addMessages newLog
    where (dungeonsAfterNpcTurns, newLog) = runWriter $ DS.handleNpcTurns $ eh ^. dungeons

getPlayerActor :: ExploringHandler -> Maybe Actor
getPlayerActor = D.getPlayerActor . getCurrentDungeon

getPlayerPosition :: ExploringHandler -> Maybe Coord
getPlayerPosition = D.playerPosition . getCurrentDungeon

actorAt :: Coord -> ExploringHandler -> Maybe Actor
actorAt c = D.actorAt c . getCurrentDungeon

isPositionInDungeon :: Coord -> ExploringHandler -> Bool
isPositionInDungeon c = D.isPositionInDungeon c . getCurrentDungeon

addMessages :: [Message] -> ExploringHandler -> ExploringHandler
addMessages newMessages eh = eh & messageLog %~ L.addMessages newMessages

getCurrentDungeon :: ExploringHandler -> Dungeon
getCurrentDungeon eh = getFocused $ eh ^. dungeons

getMessageLog :: ExploringHandler -> MessageLog
getMessageLog eh = eh ^. messageLog
