{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module GameStatus where

import           Control.Lens                   (makeLenses, (%=), (%~), (&),
                                                 (.=), (.~), (^.), (^?!))
import           Control.Monad.Trans.State      (State, get)
import           Control.Monad.Trans.State.Lazy (put, runState)
import           Coord                          (Coord)
import           Data.Binary                    (Binary)
import           Data.List                      (find, findIndex)
import           Dungeon                        (Dungeon, initDungeon,
                                                 mapWidthAndHeight, npcs,
                                                 popPlayer)
import qualified Dungeon                        as D
import           Dungeon.Entity                 (isMonster)
import qualified Dungeon.Entity                 as E
import           Dungeon.Entity.Actions         (meleeAction, moveAction)
import           Dungeon.Entity.Behavior        (npcAction)
import           Dungeon.Predefined.BatsCave    (batsDungeon)
import           Dungeon.Predefined.GlobalMap   (globalMap)
import qualified Dungeon.Turn                   as DT
import           Dungeon.Types                  (Entity, entities, maxHp,
                                                 position, positionOnGlobalMap,
                                                 talkMessage)
import           GHC.Generics                   (Generic)
import           Linear.V2                      (V2)
import           Log                            (MessageLog, addMessage,
                                                 addMessages)
import qualified Log                            as L
import           Scene                          (Scene, gameStartScene)
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
          } | Title
          deriving (Show, Ord, Eq, Generic)
makeLenses ''GameStatus
instance Binary GameStatus

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

playerBumpAction :: V2 Int -> State GameStatus ()
playerBumpAction offset = do
    gameStatus <- get

    let destination = case playerPosition gameStatus of
                          Just p  -> p + offset
                          Nothing -> error "The player is dead."

    case actorAt destination gameStatus of
        Just actorAtDestination -> if isMonster actorAtDestination
            then do
                let (msg, currentDungeon') = flip runState (gameStatus ^?! currentDungeon) $ do
                        p <- popPlayer
                        meleeAction offset p
                messageLog %= addMessages msg
                currentDungeon .= currentDungeon'
            else
                let tw = talkWith actorAtDestination $ actorAtDestination ^. talkMessage
                in put $ Talking { _talk = tw
                                 , _afterTalking = gameStatus
                                 }
        Nothing                 ->
            if isPositionInDungeon destination gameStatus
                then do
                    let (msg, currentDungeon') = flip runState (gameStatus ^?! currentDungeon) $ do
                            p <- popPlayer
                            moveAction offset p
                    messageLog %= addMessages msg
                    currentDungeon .= currentDungeon'
                else let (p, currentDungeon') = runState popPlayer (gameStatus ^?! currentDungeon)
                     in do
                     otherDungeons %= (:) currentDungeon'
                     let g = find D.isGlobalMap $ gameStatus ^?! otherDungeons
                         newPosition = currentDungeon' ^?! positionOnGlobalMap
                         newPlayer = p & position .~ case newPosition of
                             Just pos -> pos
                             Nothing -> error "The new position is not specified."
                     currentDungeon .= case g of
                         Just g' -> g' & entities %~ (:) newPlayer
                         Nothing -> error "Global map not found."

getPlayerEntity :: GameStatus -> Maybe Entity
getPlayerEntity (PlayerIsExploring d _ _ _) = D.getPlayerEntity d
getPlayerEntity (Talking _ gs)              = GameStatus.getPlayerEntity gs
getPlayerEntity (HandlingScene _ gs)        = GameStatus.getPlayerEntity gs
getPlayerEntity Title                       = error "We are in the title."

playerCurrentHp :: GameStatus -> Maybe Int
playerCurrentHp gs = E.getHp <$> getPlayerEntity gs

playerMaxHp :: GameStatus -> Maybe Int
playerMaxHp gs = (^. maxHp) <$> getPlayerEntity gs

playerPosition :: GameStatus -> Maybe Coord
playerPosition (PlayerIsExploring d _ _ _) = D.playerPosition d
playerPosition (Talking _ e)               = playerPosition e
playerPosition (HandlingScene _ e)         = playerPosition e
playerPosition Title                       = error "unreachable."

actorAt :: Coord -> GameStatus -> Maybe E.Entity
actorAt c (PlayerIsExploring d _ _ _) = D.actorAt c d
actorAt c (Talking _ e)               = actorAt c e
actorAt c (HandlingScene _ e)         = actorAt c e
actorAt _ Title                       = error "We are in the title."

isPositionInDungeon :: Coord -> GameStatus -> Bool
isPositionInDungeon c (PlayerIsExploring d _ _ _) = D.isPositionInDungeon c d
isPositionInDungeon c (Talking _ e)               = isPositionInDungeon c e
isPositionInDungeon c (HandlingScene _ e)         = isPositionInDungeon c e
isPositionInDungeon _ Title                       = error "We are in the title."

currentMapWidthAndHeight :: GameStatus -> V2 Int
currentMapWidthAndHeight (PlayerIsExploring d _ _ _) = mapWidthAndHeight d
currentMapWidthAndHeight (Talking _ e)             = currentMapWidthAndHeight e
currentMapWidthAndHeight (HandlingScene _ e)       = currentMapWidthAndHeight e
currentMapWidthAndHeight _                         = error "unreachable."

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

messageLogList :: GameStatus -> MessageLog
messageLogList (PlayerIsExploring _ _ l _) = l
messageLogList (Talking _ e)               = messageLogList e
messageLogList (HandlingScene _ e)         = messageLogList e
messageLogList Title                       = error "no message log."

newGameGameStatus :: IO GameStatus
newGameGameStatus = do
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
