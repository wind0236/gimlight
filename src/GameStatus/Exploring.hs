{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module GameStatus.Exploring
    ( ExploringHandler
    , exploringHandler
    , ascendStairsAtPlayerPosition
    , descendStairsAtPlayerPosition
    , exitDungeon
    , doPlayerAction
    , processAfterPlayerTurn
    , updateQuests
    , getQuests
    , getPlayerActor
    , getPlayerPosition
    , isPositionInDungeon
    , getCurrentDungeon
    , getMessageLog
    ) where

import           Action                        (Action, ActionStatus)
import           Actor                         (Actor, getIdentifier)
import           Control.Lens                  (makeLenses, (%~), (&), (.~),
                                                (^.))
import           Control.Monad.Trans.Writer    (runWriter)
import           Coord                         (Coord)
import           Data.Binary                   (Binary)
import           Dungeon                       (Dungeon)
import qualified Dungeon                       as D
import           Dungeon.Map.Tile              (TileCollection)
import           GHC.Generics                  (Generic)
import           GameStatus.Exploring.Dungeons (Dungeons)
import qualified GameStatus.Exploring.Dungeons as DS
import           Log                           (MessageLog)
import qualified Log                           as L
import           Quest                         (QuestCollection,
                                                handleWithTurnResult)
import           TreeZipper                    (TreeZipper, getFocused, modify)

data ExploringHandler =
    ExploringHandler
        { _dungeons       :: Dungeons
        , _messageLog     :: MessageLog
        , _quests         :: QuestCollection
        , _tileCollection :: TileCollection
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''ExploringHandler

instance Binary ExploringHandler

exploringHandler ::
       TreeZipper Dungeon
    -> MessageLog
    -> QuestCollection
    -> TileCollection
    -> ExploringHandler
exploringHandler = ExploringHandler

ascendStairsAtPlayerPosition :: ExploringHandler -> Maybe ExploringHandler
ascendStairsAtPlayerPosition eh =
    (\x -> eh & dungeons .~ x) <$>
    DS.ascendStairsAtPlayerPosition (eh ^. tileCollection) (eh ^. dungeons)

descendStairsAtPlayerPosition :: ExploringHandler -> Maybe ExploringHandler
descendStairsAtPlayerPosition eh =
    (\x -> eh & dungeons .~ x) <$>
    DS.descendStairsAtPlayerPosition (eh ^. tileCollection) (eh ^. dungeons)

exitDungeon :: ExploringHandler -> Maybe ExploringHandler
exitDungeon eh = (\x -> eh & dungeons .~ x) <$> DS.exitDungeon (eh ^. dungeons)

doPlayerAction :: Action -> ExploringHandler -> (ActionStatus, ExploringHandler)
doPlayerAction action eh = (status, newHandler)
  where
    ((status, dungeonsAfterAction, killed), newLog) =
        runWriter $
        DS.doPlayerAction action (eh ^. tileCollection) (eh ^. dungeons)
    newHandler =
        eh & messageLog %~ L.addMessages newLog &
        dungeons .~ dungeonsAfterAction &
        quests %~
        handleWithTurnResult
            (D.getIdentifier (getFocused dungeonsAfterAction))
            (map getIdentifier killed)

processAfterPlayerTurn :: ExploringHandler -> Maybe ExploringHandler
processAfterPlayerTurn eh =
    (\x ->
         handlerAfterNpcTurns & dungeons %~ modify (const x) &
         quests %~ updateQuestsForResult (D.getIdentifier x)) <$>
    newCurrentDungeon
  where
    updateQuestsForResult d = handleWithTurnResult d $ map getIdentifier killed
    newCurrentDungeon =
        D.updateMap (eh ^. tileCollection) $
        getFocused $ handlerAfterNpcTurns ^. dungeons
    (handlerAfterNpcTurns, killed) = handleNpcTurns eh

handleNpcTurns :: ExploringHandler -> (ExploringHandler, [Actor])
handleNpcTurns eh = (newHandler, killed)
  where
    newHandler =
        eh & dungeons .~ dungeonsAfterNpcTurns &
        messageLog %~ L.addMessages newLog
    ((dungeonsAfterNpcTurns, killed), newLog) =
        runWriter $ DS.handleNpcTurns (eh ^. tileCollection) (eh ^. dungeons)

updateQuests :: QuestCollection -> ExploringHandler -> ExploringHandler
updateQuests q e = e & quests .~ q

getQuests :: ExploringHandler -> QuestCollection
getQuests e = e ^. quests

getPlayerActor :: ExploringHandler -> Maybe Actor
getPlayerActor = D.getPlayerActor . getCurrentDungeon

getPlayerPosition :: ExploringHandler -> Maybe Coord
getPlayerPosition = D.playerPosition . getCurrentDungeon

isPositionInDungeon :: Coord -> ExploringHandler -> Bool
isPositionInDungeon c = D.isPositionInDungeon c . getCurrentDungeon

getCurrentDungeon :: ExploringHandler -> Dungeon
getCurrentDungeon eh = getFocused $ eh ^. dungeons

getMessageLog :: ExploringHandler -> MessageLog
getMessageLog eh = eh ^. messageLog
