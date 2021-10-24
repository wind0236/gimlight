{-# LANGUAGE DeriveGeneric #-}

module Game.Status.Exploring
    ( ExploringHandler
    , exploringHandler
    , enterTownAtPlayerPosition
    , exitDungeon
    , doAction
    , completeThisTurn
    , getPlayerActor
    , getPlayerPosition
    , actorAt
    , isPositionInDungeon
    , pushDungeonAsOtherDungeons
    , popDungeonAt
    , addMessages
    , getCurrentDungeon
    , getOtherDungeons
    , getMessageLog
    ) where

import           Control.Lens              ((%~), (&), (.~), (^.))
import           Control.Monad.Trans.State (execState, runState)
import           Coord                     (Coord)
import           Data.Binary               (Binary)
import           Data.List                 (find, findIndex)
import           Dungeon                   (Dungeon, actors,
                                            initialPlayerPositionCandidates,
                                            isGlobalMap, npcs, popPlayer,
                                            positionOnGlobalMap, updateMap)
import qualified Dungeon                   as D
import           Dungeon.Actor             (Actor, position)
import           Dungeon.Actor.Actions     (Action)
import           Dungeon.Actor.Behavior    (npcAction)
import           Dungeon.Turn              (Status (PlayerKilled))
import           GHC.Generics              (Generic)
import           Log                       (Message, MessageLog)
import qualified Log                       as L

data ExploringHandler = ExploringHandler
                      { currentDungeon :: Dungeon
                      , otherDungeons  :: [Dungeon]
                      , messageLog     :: MessageLog
                      } deriving (Show, Ord, Eq, Generic)

instance Binary ExploringHandler

exploringHandler :: Dungeon -> [Dungeon] -> MessageLog -> ExploringHandler
exploringHandler = ExploringHandler

enterTownAtPlayerPosition :: ExploringHandler -> ExploringHandler
enterTownAtPlayerPosition eh =
    case popDungeonAtPlayerPosition eh of
        (Just d, exploringHandlerWithoutNextDungeon) ->
            let newPosition = head $ initialPlayerPositionCandidates d
                (p, currentDungeonWithoutPlayer) = runState popPlayer $ getCurrentDungeon eh
                newOtherDungeons = currentDungeonWithoutPlayer:getOtherDungeons exploringHandlerWithoutNextDungeon
                newCurrentDungeon = execState updateMap $ d & actors %~ (:) (p & position .~ newPosition)
            in eh { currentDungeon = newCurrentDungeon, otherDungeons = newOtherDungeons }
        (Nothing, _) -> eh

exitDungeon :: ExploringHandler -> Maybe ExploringHandler
exitDungeon eh = fmap (\cd -> eh { currentDungeon = cd, otherDungeons = getOtherDungeons handlerWithNewOtherDungeons  } ) newCurrentDungeon
    where (player, currentDungeonWithoutPlayer) = runState popPlayer $ getCurrentDungeon eh
          handlerWithNewOtherDungeons = pushDungeonAsOtherDungeons currentDungeonWithoutPlayer eh
          globalMap = find isGlobalMap $ getOtherDungeons handlerWithNewOtherDungeons
          newPosition = getCurrentDungeon eh ^. positionOnGlobalMap
          newPlayer = case newPosition of
                          Just pos -> Just $ player & position .~ pos
                          Nothing  -> Nothing
          newCurrentDungeon = case (globalMap, newPlayer) of
                               (Just g, Just p) -> Just $ g & actors %~ (:) p
                               _                -> Nothing

doAction :: Action -> ExploringHandler -> (Bool, ExploringHandler)
doAction action eh = (isSuccess, newHandler)
    where ((newLogs, isSuccess), newCurrentDungeon) = flip runState (getCurrentDungeon eh) $ do
            p <- popPlayer
            action p
          handlerWithNewLog = addMessages newLogs eh
          newHandler = handlerWithNewLog { currentDungeon = newCurrentDungeon }

completeThisTurn :: ExploringHandler -> Maybe ExploringHandler
completeThisTurn eh = if status == PlayerKilled
                          then Nothing
                          else Just handlerAfterNpcTurns { currentDungeon = newCurrentDungeon }
    where handlerAfterNpcTurns = handleNpcTurns eh
          (status, newCurrentDungeon) = runState D.completeThisTurn $ getCurrentDungeon handlerAfterNpcTurns

handleNpcTurns :: ExploringHandler -> ExploringHandler
handleNpcTurns eh = foldl (\acc x -> handleNpcTurn (x ^. position) acc) eh $ npcs $ getCurrentDungeon eh

handleNpcTurn :: Coord -> ExploringHandler -> ExploringHandler
handleNpcTurn c eh = eh { currentDungeon = newCurrentDungeon, messageLog = newMessageLog }
    where newMessageLog = L.addMessages generatedLog (getMessageLog eh)
          (generatedLog, newCurrentDungeon) = flip runState (getCurrentDungeon eh) $ do
            dungeonWithoutActor <- D.popActorAt c
            case dungeonWithoutActor of
                Just d  -> npcAction d
                Nothing -> error "No such enemy"

getPlayerActor :: ExploringHandler -> Maybe Actor
getPlayerActor = D.getPlayerActor . getCurrentDungeon

getPlayerPosition :: ExploringHandler -> Maybe Coord
getPlayerPosition = D.playerPosition . getCurrentDungeon

actorAt :: Coord -> ExploringHandler -> Maybe Actor
actorAt c = D.actorAt c . getCurrentDungeon

isPositionInDungeon :: Coord -> ExploringHandler -> Bool
isPositionInDungeon c = D.isPositionInDungeon c . getCurrentDungeon

pushDungeonAsOtherDungeons :: Dungeon -> ExploringHandler -> ExploringHandler
pushDungeonAsOtherDungeons d e@ExploringHandler { otherDungeons = os } =
    e { otherDungeons = d:os }

popDungeonAtPlayerPosition :: ExploringHandler -> (Maybe Dungeon, ExploringHandler)
popDungeonAtPlayerPosition eh = case getPlayerPosition eh of
                                   Just p  -> popDungeonAt p eh
                                   Nothing -> (Nothing, eh)

popDungeonAt :: Coord -> ExploringHandler -> (Maybe Dungeon, ExploringHandler)
popDungeonAt c eh =
    case findIndex (\x -> x ^. positionOnGlobalMap == Just c) $ getOtherDungeons eh of
        Just x -> let ds = getOtherDungeons eh
                      d = ds !! x
                      newDungeons = take x ds ++ drop (x + 1) ds
                  in (Just d, eh { otherDungeons = newDungeons })
        Nothing -> (Nothing, eh)

addMessages :: [Message] -> ExploringHandler -> ExploringHandler
addMessages newMessages eh = eh { messageLog = L.addMessages newMessages $ getMessageLog eh }

getCurrentDungeon :: ExploringHandler -> Dungeon
getCurrentDungeon ExploringHandler { currentDungeon = c } = c

getOtherDungeons :: ExploringHandler -> [Dungeon]
getOtherDungeons ExploringHandler { otherDungeons = o } = o

getMessageLog :: ExploringHandler -> MessageLog
getMessageLog ExploringHandler { messageLog = l } = l
