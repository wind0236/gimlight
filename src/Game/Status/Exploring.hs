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

import           Control.Lens           ((%~), (&), (.~), (^.))
import           Coord                  (Coord)
import           Data.Binary            (Binary)
import           Data.Foldable          (find)
import           Dungeon                (Dungeon, actors, ascendingStairs,
                                         descendingStairs, npcs, popPlayer,
                                         positionOnParentMap, updateMap)
import qualified Dungeon                as D
import           Dungeon.Actor          (Actor, position)
import           Dungeon.Actor.Actions  (Action)
import           Dungeon.Actor.Behavior (npcAction)
import           Dungeon.Stairs         (StairsPair (StairsPair, downStairs, upStairs))
import           Dungeon.Turn           (Status (PlayerKilled))
import           GHC.Generics           (Generic)
import           Log                    (Message, MessageLog)
import qualified Log                    as L
import           TreeZipper             (TreeZipper, getFocused, goDownBy, goUp,
                                         modify)

data ExploringHandler = ExploringHandler
                      { dungeons   :: TreeZipper Dungeon
                      , messageLog :: MessageLog
                      } deriving (Show, Ord, Eq, Generic)

instance Binary ExploringHandler

exploringHandler :: TreeZipper Dungeon -> MessageLog -> ExploringHandler
exploringHandler = ExploringHandler

ascendStairsAtPlayerPosition :: ExploringHandler -> Maybe ExploringHandler
ascendStairsAtPlayerPosition eh@ExploringHandler { dungeons = ds } =
        fmap (\x -> eh { dungeons = x }) newZipper
    where (player, zipperWithoutPlayer) = popPlayerFromZipper ds
          newPlayer = case (player, newPosition) of
                          (Just p, Just pos) -> Just $ p & position .~ pos
                          _                  -> Nothing
          ascendable = (downStairs <$> getFocused ds ^. ascendingStairs) == fmap (^. position) player
          zipperFocusingNextDungeon = goUp zipperWithoutPlayer
          newPosition = upStairs <$> getFocused ds ^. ascendingStairs
          newZipper = case (zipperFocusingNextDungeon, newPlayer, ascendable) of
                          (Just g, Just p, True) ->
                                Just $ modify (\d -> updateMap $ d & actors %~ (:) p) g
                          _ -> Nothing

descendStairsAtPlayerPosition :: ExploringHandler -> Maybe ExploringHandler
descendStairsAtPlayerPosition eh@ExploringHandler{ dungeons = ds } =
    fmap (\x -> eh { dungeons = x }) newZipper
    where (player, zipperWithoutPlayer) = popPlayerFromZipper ds
          newPlayer = case (player, newPosition) of
                          (Just p, Just pos) -> Just $ p & position .~ pos
                          _                  -> Nothing
          zipperFocusingNextDungeon = goDownBy (\x -> x ^. positionOnParentMap == fmap (^. position) player) zipperWithoutPlayer
          newPosition = downStairs <$> find (\(StairsPair from _) -> Just from == fmap (^. position) player) (getFocused ds ^. descendingStairs)
          newZipper = case (zipperFocusingNextDungeon, newPlayer) of
                          (Just g, Just p) -> Just $ modify (\d -> updateMap $ d & actors %~ (:) p) g
                          _ -> Nothing

popPlayerFromZipper :: TreeZipper Dungeon -> (Maybe Actor, TreeZipper Dungeon)
popPlayerFromZipper z = (fst $ popPlayer $ getFocused z, modify (snd . popPlayer) z)

exitDungeon :: ExploringHandler -> Maybe ExploringHandler
exitDungeon eh@ExploringHandler { dungeons = ds } =
    fmap (\ds' -> eh { dungeons = ds' }) newZipper
    where zipperWithoutPlayer = modify (snd . popPlayer) ds
          currentDungeon = getFocused ds
          player = fst $ popPlayer currentDungeon
          newPosition = currentDungeon ^. positionOnParentMap
          newPlayer = case (player, newPosition) of
                          (Just p, Just pos) -> Just $ p & position .~ pos
                          _                  -> Nothing
          zipperFocusingGlobalMap = goUp zipperWithoutPlayer
          newZipper = case (zipperFocusingGlobalMap, newPlayer) of
                          (Just g, Just p) -> Just $ modify (\d -> d & actors %~ (:) p) g
                          _                -> Nothing

doAction :: Action -> ExploringHandler -> (Bool, ExploringHandler)
doAction action eh@ExploringHandler { dungeons = ds } = result
    where currentDungeon = getFocused ds
          (player, dungeonWithoutPlayer) = popPlayer currentDungeon
          result = case player of
                    Just p -> let ((newLogs, isSuccess), newCurrentDungeon) =
                                    action p dungeonWithoutPlayer
                                  handlerWithNewLog =
                                    addMessages newLogs eh
                                  newHandler =
                                    handlerWithNewLog { dungeons = modify (const newCurrentDungeon) ds }
                              in (isSuccess, newHandler)
                    Nothing -> (False, eh)

completeThisTurn :: ExploringHandler -> Maybe ExploringHandler
completeThisTurn eh =
    if status == PlayerKilled
        then Nothing
        else Just handlerAfterNpcTurns { dungeons = modify (const newCurrentDungeon) $ dungeons handlerAfterNpcTurns }
    where handlerAfterNpcTurns = handleNpcTurns eh
          (status, newCurrentDungeon) = D.completeThisTurn $ getFocused $ dungeons handlerAfterNpcTurns

handleNpcTurns :: ExploringHandler -> ExploringHandler
handleNpcTurns eh = foldl (\acc x -> handleNpcTurn (x ^. position) acc) eh $ npcs $ getCurrentDungeon eh

handleNpcTurn :: Coord -> ExploringHandler -> ExploringHandler
handleNpcTurn c eh@ExploringHandler { dungeons = ds } = newHandler
    where dungeonsWithoutTheActor = modify (snd . D.popActorAt c) ds
          theActor = fst . D.popActorAt c $ getFocused ds
          newHandler = case theActor of
                           Just x ->
                            case npcAction x $ getFocused dungeonsWithoutTheActor of
                                Just (generatedLog, newCurrentDungeon) ->
                                    let newMessageLog = L.addMessages generatedLog (getMessageLog eh)
                                    in eh { dungeons = modify (const newCurrentDungeon) dungeonsWithoutTheActor
                                          , messageLog = newMessageLog
                                          }
                                Nothing -> eh
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
