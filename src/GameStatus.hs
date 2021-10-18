{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module GameStatus
    ( GameStatus
    , handlePlayerMoving
    , handlePlayerPickingUp
    , handlePlayerSelectingItemToUse
    , handlePlayerConsumeItem
    , isPlayerExploring
    , isPlayerTalking
    , isHandlingScene
    , isSelectingItemToUse
    , isTitle
    , nextSceneElementOrFinish
    , enterTownAtPlayerPosition
    , finishTalking
    , finishSelecting
    , selectPrevItem
    , selectNextItem
    , getItems
    , getSelectingIndex
    , newGameStatus
    , getCurrentDungeon
    , destructTalking
    , destructHandlingScene
    , getPlayerActor
    , messageLogList
    , title
    ) where

import           Control.Lens                   (makeLensesFor, (%=), (%~), (&),
                                                 (&~), (.=), (.~), (^.), (^?!))
import           Control.Monad                  (unless, when)
import           Control.Monad.Trans.State      (State, execState, get)
import           Control.Monad.Trans.State.Lazy (put, runState)
import           Coord                          (Coord)
import           Data.Binary                    (Binary)
import           Data.List                      (find, findIndex)
import           Dungeon                        (Dungeon, actors,
                                                 initialPlayerPositionCandidates,
                                                 isTown, npcs, popPlayer,
                                                 positionOnGlobalMap, updateMap)
import qualified Dungeon                        as D
import           Dungeon.Actor                  (Actor, isMonster, position,
                                                 talkMessage)
import qualified Dungeon.Actor                  as A
import qualified Dungeon.Actor                  as E
import           Dungeon.Actor.Actions          (Action, consumeAction,
                                                 meleeAction, moveAction,
                                                 pickUpAction)
import           Dungeon.Actor.Behavior         (npcAction)
import           Dungeon.Init                   (initDungeon)
import           Dungeon.Item                   (Item)
import           Dungeon.Predefined.BatsCave    (batsDungeon)
import           Dungeon.Predefined.GlobalMap   (globalMap)
import qualified Dungeon.Turn                   as DT
import           GHC.Generics                   (Generic)
import           Linear.V2                      (V2)
import           Log                            (MessageLog, addMessage,
                                                 addMessages)
import qualified Log                            as L
import           Scene                          (Scene, elements,
                                                 gameStartScene)
import           System.Random                  (getStdGen)
import           Talking                        (TalkWith, talkWith)

data GameStatus = PlayerIsExploring
          { _currentDungeon :: Dungeon
          , _otherDungeons  :: [Dungeon]
          , _messageLog     :: MessageLog
          , _isGameOver     :: Bool
          } | Talking
          { _talk         :: TalkWith
          , _afterTalking :: GameStatus
          } | HandlingScene
          { _scene       :: Scene
          , _afterFinish :: GameStatus
          } | SelectingItemToUse
          { _items          :: [Item]
          , _selecting      :: Int
          , _afterSelecting :: GameStatus
          } | Title
          deriving (Show, Ord, Eq, Generic)
makeLensesFor [ ("_currentDungeon", "currentDungeon")
              , ("_otherDungeons", "otherDungeons")
              , ("_messageLog", "messageLog")
              , ("_isGameOver", "isGameOver")
              , ("_selecting", "selecting")
              ] ''GameStatus
instance Binary GameStatus

handlePlayerMoving :: V2 Int -> State GameStatus ()
handlePlayerMoving offset = do
    eng <- get
    let finished = eng ^?! isGameOver
    unless finished $ do
        success <- playerBumpAction offset

        when success $ do
            eng' <- get
            case eng' of
                PlayerIsExploring {} -> completeThisTurn
                _                    -> return ()

handlePlayerPickingUp :: State GameStatus ()
handlePlayerPickingUp = do
    eng <- get
    let finished = eng ^?! isGameOver
    unless finished $ do
        success <- doAction pickUpAction

        when success $ do
            eng' <- get
            case eng' of
                PlayerIsExploring {} -> completeThisTurn
                _                    -> return ()

handlePlayerSelectingItemToUse :: GameStatus -> GameStatus
handlePlayerSelectingItemToUse e =
    SelectingItemToUse { _items = xs
                       , _selecting = 0
                       , _afterSelecting = e
                       }
    where xs = A.getItems p
          p = case getPlayerActor e of
                Just x  -> x
                Nothing -> error "Player is dead."

handlePlayerConsumeItem :: State GameStatus ()
handlePlayerConsumeItem = do
    gs <- get

    unless (isSelectingListEmpty gs) $ do
        let n = getSelectingIndex gs

        put $ finishSelecting gs

        success <- doAction $ consumeAction n

        when success $ do
            completeThisTurn

isPlayerExploring :: GameStatus -> Bool
isPlayerExploring PlayerIsExploring{} = True
isPlayerExploring _                   = False

isPlayerTalking :: GameStatus -> Bool
isPlayerTalking Talking{} = True
isPlayerTalking _         = False

isHandlingScene :: GameStatus -> Bool
isHandlingScene HandlingScene{} = True
isHandlingScene _               = False

isSelectingItemToUse :: GameStatus -> Bool
isSelectingItemToUse SelectingItemToUse{} = True
isSelectingItemToUse _                    = False

isTitle :: GameStatus -> Bool
isTitle Title = True
isTitle _     = False

nextSceneElementOrFinish :: GameStatus -> GameStatus
nextSceneElementOrFinish (HandlingScene s after) = if length (s ^. elements) == 1
                                                    then after
                                                    else HandlingScene (s & elements %~ tail) after
nextSceneElementOrFinish _                   = error "We are not handling a scene."

enterTownAtPlayerPosition :: GameStatus -> GameStatus
enterTownAtPlayerPosition e =
    case popDungeonAtPlayerPosition e of
        (Just d, e') -> e' &~ do
            let newPosition = head $ initialPlayerPositionCandidates d
                (p, currentDungeon') = runState popPlayer (e ^?! currentDungeon)
            otherDungeons %= (:) currentDungeon'
            currentDungeon .= (d & actors %~ (:) (p & position .~ newPosition))
            currentDungeon %= execState updateMap
        (Nothing, _) -> e

finishTalking :: GameStatus -> GameStatus
finishTalking (Talking _ after) = after
finishTalking _                 = error "We are not in the talking."

finishSelecting :: GameStatus -> GameStatus
finishSelecting (SelectingItemToUse _ _ after) = after
finishSelecting _ = error "We are not selecting anything."

selectPrevItem :: GameStatus -> GameStatus
selectPrevItem e@(SelectingItemToUse l n _)
    | null l = e
    | otherwise = e & selecting .~ newIndex
    where newIndex = (n - 1) `mod` length l
selectPrevItem _ = error "We are not selecting anything."

selectNextItem :: GameStatus -> GameStatus
selectNextItem e@(SelectingItemToUse l n _)
    | null l = e
    | otherwise = e & selecting .~ newIndex
    where newIndex = (n + 1) `mod` length l
selectNextItem _ = error "We are not selecting anything."

getItems :: GameStatus -> [Item]
getItems (SelectingItemToUse i _ _) = i
getItems _                          = error "We are not selecting anything."

getSelectingIndex :: GameStatus -> Int
getSelectingIndex (SelectingItemToUse _ n _) = n
getSelectingIndex _ = error "We are not selecting anything."

newGameStatus :: IO GameStatus
newGameStatus = do
    g <- getStdGen

    let bats = batsDungeon g
        initPlayerIsExploring = PlayerIsExploring
            { _currentDungeon = initDungeon
            , _otherDungeons = [globalMap, bats]
            , _messageLog = foldr (addMessage . L.message) L.emptyLog ["Welcome to a roguelike game!"]
            , _isGameOver = False
            }
    return HandlingScene
        { _scene = gameStartScene
        , _afterFinish = initPlayerIsExploring
        }

getCurrentDungeon :: GameStatus -> Dungeon
getCurrentDungeon (PlayerIsExploring d _ _ _)    = d
getCurrentDungeon (Talking _ after)              = getCurrentDungeon after
getCurrentDungeon (HandlingScene _ after)        = getCurrentDungeon after
getCurrentDungeon (SelectingItemToUse _ _ after) = getCurrentDungeon after
getCurrentDungeon Title                          = error "We are in the title."

destructTalking :: GameStatus -> (TalkWith, GameStatus)
destructTalking (Talking tw after) = (tw, after)
destructTalking _                  = error "We are not in the talking."

destructHandlingScene :: GameStatus -> (Scene, GameStatus)
destructHandlingScene (HandlingScene s after) = (s, after)
destructHandlingScene _ = error "We are not handling a scene."

messageLogList :: GameStatus -> MessageLog
messageLogList (PlayerIsExploring _ _ l _)    = l
messageLogList (Talking _ e)                  = messageLogList e
messageLogList (HandlingScene _ e)            = messageLogList e
messageLogList (SelectingItemToUse _ _ after) = messageLogList after
messageLogList Title                          = error "no message log."

title :: GameStatus
title = Title

completeThisTurn :: State GameStatus ()
completeThisTurn = do
        handleNpcTurns

        e <- get
        let dg = e ^?! currentDungeon

        let (status, newD) = runState D.completeThisTurn dg

        isGameOver .= (status == DT.PlayerKilled)

        currentDungeon .= newD

handleNpcTurns :: State GameStatus ()
handleNpcTurns = do
        e <- get
        let dg = e ^?! currentDungeon

        let xs = npcs dg

        mapM_ (handleNpcTurn . (^. position)) xs

handleNpcTurn :: Coord -> State GameStatus ()
handleNpcTurn c = do
        e <- get
        let dg = e ^?! currentDungeon

        let (l, dg') = flip runState dg $ do
                e' <- D.popActorAt c
                case e' of
                    Just e'' -> npcAction e''
                    Nothing  -> error "No such enemy."

        messageLog %= addMessages l
        currentDungeon .= dg'

playerBumpAction :: V2 Int -> State GameStatus Bool
playerBumpAction offset = do
    gameStatus <- get

    let destination = case playerPosition gameStatus of
                          Just p  -> p + offset
                          Nothing -> error "The player is dead."

    case actorAt destination gameStatus of
        Just actorAtDestination -> meleeOrTalk offset actorAtDestination
        Nothing                 ->
            if isPositionInDungeon destination gameStatus || not (isTown (gameStatus ^?! currentDungeon))
                then doAction $ moveAction offset
                else let (p, currentDungeon') = runState popPlayer (gameStatus ^?! currentDungeon)
                     in do
                     otherDungeons %= (:) currentDungeon'
                     let g = find D.isGlobalMap $ gameStatus ^?! otherDungeons
                         newPosition = currentDungeon' ^?! positionOnGlobalMap
                         newPlayer = p & position .~ case newPosition of
                             Just pos -> pos
                             Nothing -> error "The new position is not specified."
                     currentDungeon .= case g of
                         Just g' -> g' & actors %~ (:) newPlayer
                         Nothing -> error "Global map not found."
                     return True

meleeOrTalk :: V2 Int -> Actor -> State GameStatus Bool
meleeOrTalk offset target = do
    gameStatus <- get

    if isMonster target
        then doAction $ meleeAction offset
        else do
            put $ Talking { _talk = talkWith target $ target ^. talkMessage
                          , _afterTalking = gameStatus
                          }
            return True

doAction :: Action -> State GameStatus Bool
doAction action = do
    gs <- get

    let ((msg, success), currentDungeon') = flip runState (gs ^?! currentDungeon) $ do
            p <- popPlayer
            action p
    messageLog %= addMessages msg
    currentDungeon .= currentDungeon'
    return success

getPlayerActor :: GameStatus -> Maybe Actor
getPlayerActor (PlayerIsExploring d _ _ _) = D.getPlayerActor d
getPlayerActor (Talking _ gs)              = getPlayerActor gs
getPlayerActor (HandlingScene _ gs)        = getPlayerActor gs
getPlayerActor (SelectingItemToUse _ _ gs) = getPlayerActor gs
getPlayerActor Title                       = error "We are in the title."

playerPosition :: GameStatus -> Maybe Coord
playerPosition (PlayerIsExploring d _ _ _) = D.playerPosition d
playerPosition (Talking _ e)               = playerPosition e
playerPosition (HandlingScene _ e)         = playerPosition e
playerPosition (SelectingItemToUse _ _ gs) = playerPosition gs
playerPosition Title                       = error "unreachable."

actorAt :: Coord -> GameStatus -> Maybe E.Actor
actorAt c (PlayerIsExploring d _ _ _) = D.actorAt c d
actorAt c (Talking _ e)               = actorAt c e
actorAt c (HandlingScene _ e)         = actorAt c e
actorAt c (SelectingItemToUse _ _ e)  = actorAt c e
actorAt _ Title                       = error "We are in the title."

isPositionInDungeon :: Coord -> GameStatus -> Bool
isPositionInDungeon c (PlayerIsExploring d _ _ _) = D.isPositionInDungeon c d
isPositionInDungeon c (Talking _ e)               = isPositionInDungeon c e
isPositionInDungeon c (HandlingScene _ e)         = isPositionInDungeon c e
isPositionInDungeon c (SelectingItemToUse _ _ e)  = isPositionInDungeon c e
isPositionInDungeon _ Title                       = error "We are in the title."

popDungeonAtPlayerPosition :: GameStatus -> (Maybe Dungeon, GameStatus)
popDungeonAtPlayerPosition g = case playerPosition g of
                                   Just p  -> popDungeonAt p g
                                   Nothing -> (Nothing, g)

popDungeonAt :: Coord -> GameStatus -> (Maybe Dungeon, GameStatus)
popDungeonAt p e = let xs = e ^. otherDungeons
                   in case findIndex (\x -> x ^. positionOnGlobalMap == Just p) xs of
                          Just x -> let d = xs !! x
                                        newOtherDungeons = take x xs ++ drop (x + 1) xs
                                    in (Just d, e & otherDungeons .~ newOtherDungeons)
                          Nothing -> (Nothing, e)

isSelectingListEmpty :: GameStatus -> Bool
isSelectingListEmpty (SelectingItemToUse l _ _) = null l
isSelectingListEmpty _ = error "We are not selecting anything."
