{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module GameStatus
    ( GameStatus
    , isPlayerExploring
    , isPlayerTalking
    , isHandlingScene
    , isSelectingItemToUse
    , isTitle
    , isGameOver
    , completeThisTurn
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
    , getOtherDungeons
    , currentDungeon
    , destructTalking
    , destructHandlingScene
    , getPlayerActor
    , messageLogList
    , title
    , talking
    , selectingItemToUse
    , addMessages
    , actorAt
    , playerPosition
    , isPositionInDungeon
    , pushDungeonAsOtherDungeons
    , isSelectingListEmpty
    ) where

import           Control.Lens                   (makeLensesFor, (%=), (%~), (&),
                                                 (&~), (.=), (.~), (^.), (^?!))
import           Control.Monad.Trans.State      (State, execState, get, state)
import           Control.Monad.Trans.State.Lazy (runState)
import           Coord                          (Coord)
import           Data.Binary                    (Binary)
import           Data.List                      (findIndex)
import           Dungeon                        (Dungeon, actors,
                                                 initialPlayerPositionCandidates,
                                                 npcs, popPlayer,
                                                 positionOnGlobalMap, updateMap)
import qualified Dungeon                        as D
import           Dungeon.Actor                  (Actor, position)
import qualified Dungeon.Actor                  as E
import           Dungeon.Actor.Behavior         (npcAction)
import           Dungeon.Init                   (initDungeon)
import           Dungeon.Item                   (Item)
import           Dungeon.Predefined.BatsCave    (batsDungeon)
import           Dungeon.Predefined.GlobalMap   (globalMap)
import qualified Dungeon.Turn                   as DT
import           GHC.Generics                   (Generic)
import           Log                            (Message, MessageLog)
import qualified Log                            as L
import           Scene                          (Scene, elements,
                                                 gameStartScene)
import           System.Random                  (getStdGen)
import           Talking                        (TalkWith)

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

selectingItemToUse :: [Item] -> GameStatus -> GameStatus
selectingItemToUse i st = SelectingItemToUse { _items = i
                                             , _selecting = 0
                                             , _afterSelecting = st
                                             }

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
            , _messageLog = foldr (L.addMessage . L.message) L.emptyLog ["Welcome to a roguelike game!"]
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

getOtherDungeons :: GameStatus -> [Dungeon]
getOtherDungeons (PlayerIsExploring _ o _ _)    = o
getOtherDungeons (Talking _ after)              = getOtherDungeons after
getOtherDungeons (HandlingScene _ after)        = getOtherDungeons after
getOtherDungeons (SelectingItemToUse _ _ after) = getOtherDungeons after
getOtherDungeons Title                          = error "We are in tht title."

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

addMessages :: [Message] -> State GameStatus ()
addMessages m = state
    $ \case
        e@PlayerIsExploring{}       -> ((), e & messageLog %~ L.addMessages m)
        (Talking _ gs)              -> runState (addMessages m) gs
        (HandlingScene _ gs)        -> runState (addMessages m) gs
        (SelectingItemToUse _ _ gs) -> runState (addMessages m) gs
        Title                       -> error "No message log."

talking :: TalkWith -> GameStatus -> GameStatus
talking tw gs = Talking { _talk = tw
                        , _afterTalking = gs
                        }

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

        messageLog %= L.addMessages l
        currentDungeon .= dg'

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

pushDungeonAsOtherDungeons :: Dungeon -> State GameStatus ()
pushDungeonAsOtherDungeons d = state
    $ \case
        gs@PlayerIsExploring{} -> ((), gs & otherDungeons %~ (:) d)
        (Talking _ gs) -> runState (pushDungeonAsOtherDungeons d) gs
        (HandlingScene _ gs) -> runState (pushDungeonAsOtherDungeons d) gs
        (SelectingItemToUse _ _ gs) -> runState (pushDungeonAsOtherDungeons d) gs
        Title -> error "We are in the title."

isSelectingListEmpty :: GameStatus -> Bool
isSelectingListEmpty (SelectingItemToUse l _ _) = null l
isSelectingListEmpty _ = error "We are not selecting anything."
