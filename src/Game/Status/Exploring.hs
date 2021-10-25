{-# LANGUAGE DeriveGeneric #-}

module Game.Status.Exploring
    ( ExploringHandler
    , exploringHandler
    , ascendStairsAtPlayerPosition
    , descendStairsAtPlayerPosition
    , exitDungeon
    , doAction
    , completeThisTurn
    , getPlayerActor
    , getPlayerPosition
    , actorAt
    , isPositionInDungeon
    , addMessages
    , getCurrentDungeon
    , getMessageLog
    ) where

import           Control.Lens              ((%~), (&), (.~), (^.))
import           Control.Monad.Trans.State (evalState, execState, runState)
import           Coord                     (Coord)
import           Data.Binary               (Binary)
import           Data.Foldable             (find)
import           Data.Maybe                (fromMaybe)
import           Dungeon                   (Dungeon, actors, ascendingStairs,
                                            descendingStairs, npcs, popPlayer,
                                            positionOnParentMap, updateMap)
import qualified Dungeon                   as D
import           Dungeon.Actor             (Actor, position)
import           Dungeon.Actor.Actions     (Action)
import           Dungeon.Actor.Behavior    (npcAction)
import           Dungeon.Stairs            (StairsPair (StairsPair, downStairs, upStairs))
import           Dungeon.Turn              (Status (PlayerKilled))
import           GHC.Generics              (Generic)
import           Log                       (Message, MessageLog)
import qualified Log                       as L
import           TreeZipper                (TreeZipper, getFocused, goDownBy,
                                            goUp, modify)

data ExploringHandler = ExploringHandler
                      { dungeons   :: TreeZipper Dungeon
                      , messageLog :: MessageLog
                      } deriving (Show, Ord, Eq, Generic)

instance Binary ExploringHandler

exploringHandler :: TreeZipper Dungeon -> MessageLog -> ExploringHandler
exploringHandler = ExploringHandler

ascendStairsAtPlayerPosition :: ExploringHandler -> ExploringHandler
ascendStairsAtPlayerPosition eh@ExploringHandler { dungeons = ds } =
        eh { dungeons = newZipper }
    where (player, zipperWithoutPlayer) = popPlayerFromZipper ds
          newPlayer = fmap (\x -> player & position .~ x) newPosition
          ascendable = (downStairs <$> getFocused ds ^. ascendingStairs) == Just (player ^. position)
          zipperFocusingNextDungeon = goUp zipperWithoutPlayer
          newPosition = upStairs <$> getFocused ds ^. ascendingStairs
          newZipper = case (zipperFocusingNextDungeon, newPlayer, ascendable) of
                          (Just g, Just p, True) ->
                                modify (\d -> execState updateMap $ d & actors %~ (:) p) g
                          _ -> ds

descendStairsAtPlayerPosition :: ExploringHandler -> ExploringHandler
descendStairsAtPlayerPosition eh@ExploringHandler{ dungeons = ds } =
    eh { dungeons = fromMaybe ds newZipper }
    where (player, zipperWithoutPlayer) = popPlayerFromZipper ds
          newPlayer = fmap (\x -> player & position .~ x) newPosition
          zipperFocusingNextDungeon = goDownBy (\x -> x ^. positionOnParentMap == Just (player ^. position)) zipperWithoutPlayer
          newPosition = downStairs <$> find (\(StairsPair from _) -> from == player ^. position) (getFocused ds ^. descendingStairs)
          newZipper = case (zipperFocusingNextDungeon, newPlayer) of
                          (Just g, Just p) -> Just $ modify (\d -> execState updateMap $ d & actors %~ (:) p) g
                          _ -> Nothing

popPlayerFromZipper :: TreeZipper Dungeon -> (Actor, TreeZipper Dungeon)
popPlayerFromZipper z = (evalState popPlayer $ getFocused z, modify (execState popPlayer) z)

exitDungeon :: ExploringHandler -> Maybe ExploringHandler
exitDungeon eh@ExploringHandler { dungeons = ds } =
    fmap (\ds' -> eh { dungeons = ds' }) newZipper
    where zipperWithoutPlayer = modify (execState popPlayer) ds
          currentDungeon = getFocused ds
          player = evalState popPlayer currentDungeon
          newPosition = currentDungeon ^. positionOnParentMap
          newPlayer = fmap (\x -> player & position .~ x) newPosition
          zipperFocusingGlobalMap = goUp zipperWithoutPlayer
          newZipper = case (zipperFocusingGlobalMap, newPlayer) of
                          (Just g, Just p) -> Just $ modify (\d -> d & actors %~ (:) p) g
                          _                -> Nothing

doAction :: Action -> ExploringHandler -> (Bool, ExploringHandler)
doAction action eh@ExploringHandler { dungeons = ds } = (isSuccess, newHandler)
    where currentDungeon = getFocused ds
          ((newLogs, isSuccess), newCurrentDungeon) = flip runState currentDungeon $ do
              p <- popPlayer
              action p
          handlerWithNewLog = addMessages newLogs eh
          newHandler = handlerWithNewLog { dungeons = modify (const newCurrentDungeon) ds }

completeThisTurn :: ExploringHandler -> Maybe ExploringHandler
completeThisTurn eh =
    if status == PlayerKilled
        then Nothing
        else Just handlerAfterNpcTurns { dungeons = modify (const newCurrentDungeon) $ dungeons handlerAfterNpcTurns }
    where handlerAfterNpcTurns = handleNpcTurns eh
          (status, newCurrentDungeon) = runState D.completeThisTurn $ getFocused $ dungeons handlerAfterNpcTurns

handleNpcTurns :: ExploringHandler -> ExploringHandler
handleNpcTurns eh = foldl (\acc x -> handleNpcTurn (x ^. position) acc) eh $ npcs $ getCurrentDungeon eh

handleNpcTurn :: Coord -> ExploringHandler -> ExploringHandler
handleNpcTurn c eh@ExploringHandler { dungeons = ds } = newHandler
    where dungeonsWithoutTheActor = modify (execState $ D.popActorAt c) ds
          theActor = evalState (D.popActorAt c) $ getFocused ds
          newHandler = case theActor of
                           Just x ->
                            let (generatedLog, newCurrentDungeon) =
                                    runState (npcAction x) $ getFocused dungeonsWithoutTheActor
                                newMessageLog = L.addMessages generatedLog (getMessageLog eh)
                            in eh { dungeons = modify (const newCurrentDungeon) dungeonsWithoutTheActor
                                  , messageLog = newMessageLog
                                  }
                           Nothing -> error "No such npc."

getPlayerActor :: ExploringHandler -> Maybe Actor
getPlayerActor = D.getPlayerActor . getCurrentDungeon

getPlayerPosition :: ExploringHandler -> Maybe Coord
getPlayerPosition = D.playerPosition . getCurrentDungeon

actorAt :: Coord -> ExploringHandler -> Maybe Actor
actorAt c = D.actorAt c . getCurrentDungeon

isPositionInDungeon :: Coord -> ExploringHandler -> Bool
isPositionInDungeon c = D.isPositionInDungeon c . getCurrentDungeon

addMessages :: [Message] -> ExploringHandler -> ExploringHandler
addMessages newMessages eh = eh { messageLog = L.addMessages newMessages $ getMessageLog eh }

getCurrentDungeon :: ExploringHandler -> Dungeon
getCurrentDungeon ExploringHandler { dungeons = ds } = getFocused ds

getMessageLog :: ExploringHandler -> MessageLog
getMessageLog ExploringHandler { messageLog = l } = l
