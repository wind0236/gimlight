{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game.Status
    ( GameStatus(Exploring)
    , isPlayerExploring
    , isPlayerTalking
    , isHandlingScene
    , isSelectingItemToUse
    , isTitle
    , isGameOver
    , isSelectingLocale
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
    , destructTalking
    , destructHandlingScene
    , getPlayerActor
    , messageLogList
    , title
    , selectingLocale
    , talking
    , selectingItemToUse
    , addMessages
    , actorAt
    , playerPosition
    , isPositionInDungeon
    , pushDungeonAsOtherDungeons
    , isSelectingListEmpty
    ) where

import           Control.Lens                   (makeLensesFor, (%~), (&), (.~),
                                                 (^.))
import           Control.Monad.Trans.State      (State, state)
import           Control.Monad.Trans.State.Lazy (runState)
import           Coord                          (Coord)
import           Data.Binary                    (Binary)
import           Dungeon                        (Dungeon)
import           Dungeon.Actor                  (Actor)
import qualified Dungeon.Actor                  as E
import           Dungeon.Init                   (initDungeon)
import           Dungeon.Item                   (Item)
import           Dungeon.Predefined.BatsCave    (batsDungeon)
import           Dungeon.Predefined.GlobalMap   (globalMap)
import           GHC.Generics                   (Generic)
import           Game.Status.Exploring          (ExploringHandler,
                                                 exploringHandler)
import qualified Game.Status.Exploring          as GSE
import           Localization                   (multilingualText)
import           Log                            (Message, MessageLog)
import qualified Log                            as L
import           Scene                          (Scene, elements,
                                                 gameStartScene)
import           System.Random                  (getStdGen)
import           Talking                        (TalkWith)

data GameStatus = Exploring ExploringHandler
            | Talking
          { _talk         :: TalkWith
          , _afterTalking :: GameStatus
          } | HandlingScene
          { _scene       :: Scene
          , _afterFinish :: GameStatus
          } | SelectingItemToUse
          { _items          :: [Item]
          , _selecting      :: Int
          , _afterSelecting :: GameStatus
          }
          | Title
          | GameOver
          | SelectingLocale
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
isPlayerExploring Exploring{} = True
isPlayerExploring _           = False

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

isGameOver :: GameStatus -> Bool
isGameOver GameOver = True
isGameOver _        = False

isSelectingLocale :: GameStatus -> Bool
isSelectingLocale SelectingLocale = True
isSelectingLocale _               = False

nextSceneElementOrFinish :: GameStatus -> GameStatus
nextSceneElementOrFinish (HandlingScene s after) = if length (s ^. elements) == 1
                                                    then after
                                                    else HandlingScene (s & elements %~ tail) after
nextSceneElementOrFinish _                   = error "We are not handling a scene."

enterTownAtPlayerPosition :: GameStatus -> GameStatus
enterTownAtPlayerPosition (Exploring eh) = Exploring $ GSE.enterTownAtPlayerPosition eh
enterTownAtPlayerPosition _ = undefined

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
        initExploring = exploringHandler
            initDungeon
            [globalMap, bats] $
            foldr (L.addMessage . L.message) L.emptyLog
                [multilingualText "Welcome to a roguelike game!" "ローグライクゲームへようこそ！"]

    return HandlingScene
        { _scene = gameStartScene
        , _afterFinish = Exploring initExploring
        }

getCurrentDungeon :: GameStatus -> Dungeon
getCurrentDungeon (Exploring eh)      = GSE.getCurrentDungeon eh
getCurrentDungeon (Talking _ after)              = getCurrentDungeon after
getCurrentDungeon (HandlingScene _ after)        = getCurrentDungeon after
getCurrentDungeon (SelectingItemToUse _ _ after) = getCurrentDungeon after
getCurrentDungeon _ = error "Cannot get the current dungeon."

getOtherDungeons :: GameStatus -> [Dungeon]
getOtherDungeons (Exploring eh) = GSE.getOtherDungeons eh
getOtherDungeons (Talking _ after)              = getOtherDungeons after
getOtherDungeons (HandlingScene _ after)        = getOtherDungeons after
getOtherDungeons (SelectingItemToUse _ _ after) = getOtherDungeons after
getOtherDungeons _                          = error "Cannot get the non-active dungeons."

destructTalking :: GameStatus -> (TalkWith, GameStatus)
destructTalking (Talking tw after) = (tw, after)
destructTalking _                  = error "We are not in the talking."

destructHandlingScene :: GameStatus -> (Scene, GameStatus)
destructHandlingScene (HandlingScene s after) = (s, after)
destructHandlingScene _ = error "We are not handling a scene."

messageLogList :: GameStatus -> MessageLog
messageLogList (Exploring eh)      = GSE.getMessageLog eh
messageLogList (Talking _ e)                  = messageLogList e
messageLogList (HandlingScene _ e)            = messageLogList e
messageLogList (SelectingItemToUse _ _ after) = messageLogList after
messageLogList _                          = error "Cannot get the message log list."

addMessages :: [Message] -> State GameStatus ()
addMessages m = state
    $ \case
        (Exploring eh)              -> ((), Exploring $ GSE.addMessages m eh)
        (Talking _ gs)              -> runState (addMessages m) gs
        (HandlingScene _ gs)        -> runState (addMessages m) gs
        (SelectingItemToUse _ _ gs) -> runState (addMessages m) gs
        _                           -> error "Cannot add messages."

talking :: TalkWith -> GameStatus -> GameStatus
talking tw gs = Talking { _talk = tw
                        , _afterTalking = gs
                        }

title :: GameStatus
title = Title

selectingLocale :: GameStatus
selectingLocale = SelectingLocale

completeThisTurn :: State GameStatus ()
completeThisTurn = state $ \case
    Exploring eh -> ((), maybe GameOver Exploring (GSE.completeThisTurn eh))
    _            -> undefined

getPlayerActor :: GameStatus -> Maybe Actor
getPlayerActor (Exploring eh)              = GSE.getPlayerActor eh
getPlayerActor (Talking _ gs)              = getPlayerActor gs
getPlayerActor (HandlingScene _ gs)        = getPlayerActor gs
getPlayerActor (SelectingItemToUse _ _ gs) = getPlayerActor gs
getPlayerActor _                           = error "Cannot get the player data."

playerPosition :: GameStatus -> Maybe Coord
playerPosition (Exploring eh)   = GSE.getPlayerPosition eh
playerPosition (Talking _ e)               = playerPosition e
playerPosition (HandlingScene _ e)         = playerPosition e
playerPosition (SelectingItemToUse _ _ gs) = playerPosition gs
playerPosition _                       = error "Cannot get the player position."

actorAt :: Coord -> GameStatus -> Maybe E.Actor
actorAt c (Exploring eh)             = GSE.actorAt c eh
actorAt c (Talking _ e)              = actorAt c e
actorAt c (HandlingScene _ e)        = actorAt c e
actorAt c (SelectingItemToUse _ _ e) = actorAt c e
actorAt _ _                          = error "Cannot get the actor data"

isPositionInDungeon :: Coord -> GameStatus -> Bool
isPositionInDungeon c (Exploring eh)  = GSE.isPositionInDungeon c eh
isPositionInDungeon c (Talking _ e)              = isPositionInDungeon c e
isPositionInDungeon c (HandlingScene _ e)        = isPositionInDungeon c e
isPositionInDungeon c (SelectingItemToUse _ _ e) = isPositionInDungeon c e
isPositionInDungeon _ _                      = error "Cannot access to a dungeon."

pushDungeonAsOtherDungeons :: Dungeon -> State GameStatus ()
pushDungeonAsOtherDungeons d = state
    $ \case
        (Exploring eh) -> ((), Exploring $ GSE.pushDungeonAsOtherDungeons d eh)
        (Talking _ gs) -> runState (pushDungeonAsOtherDungeons d) gs
        (HandlingScene _ gs) -> runState (pushDungeonAsOtherDungeons d) gs
        (SelectingItemToUse _ _ gs) -> runState (pushDungeonAsOtherDungeons d) gs
        _ -> error "Cannot push a dungeon."

isSelectingListEmpty :: GameStatus -> Bool
isSelectingListEmpty (SelectingItemToUse l _ _) = null l
isSelectingListEmpty _ = error "We are not selecting anything."
