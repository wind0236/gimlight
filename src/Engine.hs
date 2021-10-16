{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Engine where

import           Control.Lens                   (makeLenses, (%=), (%~), (&),
                                                 (.=), (.~), (^.), (^?!))
import           Control.Monad.Trans.State      (State, execState, get)
import           Control.Monad.Trans.State.Lazy (put, runState)
import           Coord                          (Coord)
import           Data.Binary                    (Binary)
import           Data.List                      (find, findIndex)
import           Dungeon                        (Dungeon, aliveNpcs,
                                                 getPlayerEntity, initDungeon,
                                                 mapWidthAndHeight, popPlayer)
import qualified Dungeon                        as D
import           Dungeon.Entity                 (isMonster)
import qualified Dungeon.Entity                 as E
import           Dungeon.Entity.Behavior        (meleeAction, moveAction,
                                                 npcAction)
import           Dungeon.Predefined.BatsCave    (batsDungeon)
import           Dungeon.Predefined.GlobalMap   (globalMap)
import qualified Dungeon.Turn                   as DT
import           Dungeon.Types                  (entities, maxHp, position,
                                                 positionOnGlobalMap,
                                                 talkMessage)
import           GHC.Generics                   (Generic)
import           Linear.V2                      (V2)
import           Log                            (MessageLog, addMessage,
                                                 addMessages)
import qualified Log                            as L
import           Scene                          (Scene, gameStartScene)
import           System.Random                  (getStdGen)
import           Talking                        (TalkWith, talkWith)

data Engine = PlayerIsExploring
          { _currentDungeon :: Dungeon
          , _otherDungeons  :: [Dungeon]
          , _messageLog     :: MessageLog
          , _isGameOver     :: Bool
          } | Talking
          { _talk         :: TalkWith
          , _afterTalking :: Engine
          } | HandlingScene
          { _scene       :: Scene
          , _afterFinish :: Engine
          } | Title
          deriving (Show, Ord, Eq, Generic)
makeLenses ''Engine
instance Binary Engine

completeThisTurn :: State Engine ()
completeThisTurn = do
        handleNpcTurns

        e <- get
        let dg = e ^?! currentDungeon

        let (status, newD) = runState D.completeThisTurn dg

        isGameOver .= (status == DT.PlayerKilled)

        currentDungeon .= newD

handleNpcTurns :: State Engine ()
handleNpcTurns = do
        e <- get
        let dg = e ^?! currentDungeon

        let xs = aliveNpcs dg

        mapM_ (handleNpcTurn . (^. position)) xs

handleNpcTurn :: Coord -> State Engine ()
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

playerBumpAction :: V2 Int -> State Engine ()
playerBumpAction offset = do
    engine <- get

    let destination = playerPosition engine + offset

    case actorAt destination engine of
        Just actorAtDestination -> if isMonster actorAtDestination
            then do
                let (msg, currentDungeon') = flip runState (engine ^?! currentDungeon) $ do
                        p <- popPlayer
                        meleeAction p offset
                messageLog %= addMessages msg
                currentDungeon .= currentDungeon'
            else
                let tw = talkWith actorAtDestination $ actorAtDestination ^. talkMessage
                in put $ Talking { _talk = tw
                                 , _afterTalking = engine
                                 }
        Nothing                 ->
            if isPositionInDungeon destination engine
                then do
                    let currentDungeon' = flip execState (engine ^?! currentDungeon) $ do
                            p <- popPlayer
                            moveAction p offset
                    currentDungeon .= currentDungeon'
                else let (p, currentDungeon') = runState popPlayer (engine ^?! currentDungeon)
                     in do
                     otherDungeons %= (:) currentDungeon'
                     let g = find D.isGlobalMap $ engine ^?! otherDungeons
                         newPosition = currentDungeon' ^?! positionOnGlobalMap
                         newPlayer = p & position .~ case newPosition of
                             Just pos -> pos
                             Nothing -> error "The new position is not specified."
                     currentDungeon .= case g of
                         Just g' -> g' & entities %~ (:) newPlayer
                         Nothing -> error "Global map not found."

playerCurrentHp :: Engine -> Int
playerCurrentHp e = E.getHp $ getPlayerEntity (e ^?! currentDungeon)

playerMaxHp :: Engine -> Int
playerMaxHp e = getPlayerEntity (e ^?! currentDungeon) ^. maxHp

playerPosition :: Engine -> Coord
playerPosition (PlayerIsExploring d _ _ _) = D.playerPosition d
playerPosition (Talking _ e)               = playerPosition e
playerPosition (HandlingScene _ e)         = playerPosition e
playerPosition Title                       = error "unreachable."

actorAt :: Coord -> Engine -> Maybe E.Entity
actorAt c (PlayerIsExploring d _ _ _) = D.actorAt c d
actorAt c (Talking _ e)               = actorAt c e
actorAt c (HandlingScene _ e)         = actorAt c e
actorAt _ Title                       = error "We are in the title."

isPositionInDungeon :: Coord -> Engine -> Bool
isPositionInDungeon c (PlayerIsExploring d _ _ _) = D.isPositionInDungeon c d
isPositionInDungeon c (Talking _ e)               = isPositionInDungeon c e
isPositionInDungeon c (HandlingScene _ e)         = isPositionInDungeon c e
isPositionInDungeon _ Title                       = error "We are in the title."

currentMapWidthAndHeight :: Engine -> V2 Int
currentMapWidthAndHeight (PlayerIsExploring d _ _ _) = mapWidthAndHeight d
currentMapWidthAndHeight (Talking _ e)             = currentMapWidthAndHeight e
currentMapWidthAndHeight (HandlingScene _ e)       = currentMapWidthAndHeight e
currentMapWidthAndHeight _                         = error "unreachable."

popDungeonAtPlayerPosition :: Engine -> (Maybe Dungeon, Engine)
popDungeonAtPlayerPosition e = popDungeonAt (playerPosition e) e

popDungeonAt :: Coord -> Engine -> (Maybe Dungeon, Engine)
popDungeonAt p e = let xs = e ^. otherDungeons
                   in case findIndex (\x -> x ^. positionOnGlobalMap == Just p) xs of
                          Just x -> let d = xs !! x
                                        newOtherDungeons = take x xs ++ drop (x + 1) xs
                                    in (Just d, e & otherDungeons .~ newOtherDungeons)
                          Nothing -> (Nothing, e)

messageLogList :: Engine -> MessageLog
messageLogList (PlayerIsExploring _ _ l _) = l
messageLogList (Talking _ e)               = messageLogList e
messageLogList (HandlingScene _ e)         = messageLogList e
messageLogList Title                       = error "no message log."

newGameEngine :: IO Engine
newGameEngine = do
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
