{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Engine where

import           Control.Lens                   (makeLenses, (%=), (.=), (^.),
                                                 (^?!))
import           Control.Monad.Trans.State      (State, get)
import           Control.Monad.Trans.State.Lazy (put, runState)
import           Coord                          (Coord)
import           Dungeon                        (Dungeon, aliveEnemies,
                                                 getPlayerEntity, initDungeon,
                                                 mapWidthAndHeight)
import qualified Dungeon                        as D
import qualified Dungeon.Entity                 as E
import           Dungeon.Entity.Behavior        (BumpResult (..), bumpAction,
                                                 enemyAction)
import qualified Dungeon.Turn                   as DT
import           Dungeon.Types                  (maxHp, position)
import           Linear.V2                      (V2)
import           Log                            (MessageLog, addMessage,
                                                 addMessages)
import qualified Log                            as L
import           Scene                          (Scene, gameStartScene)
import           Talking                        (TalkWith)

data Engine = PlayerIsExploring
          { _dungeon    :: Dungeon
          , _messageLog :: MessageLog
          , _isGameOver :: Bool
          } | Talking
          { _talk         :: TalkWith
          , _afterTalking :: Engine
          } | HandlingScene
          { _scene       :: Scene
          , _afterFinish :: Engine
          } deriving (Show, Ord, Eq)
makeLenses ''Engine

completeThisTurn :: State Engine ()
completeThisTurn = do
        handleEnemyTurns

        e <- get
        let dg = e ^?! dungeon

        let (status, newD) = runState D.completeThisTurn dg

        isGameOver .= (status == DT.PlayerKilled)

        dungeon .= newD

handleEnemyTurns :: State Engine ()
handleEnemyTurns = do
        e <- get
        let dg = e ^?! dungeon

        let xs = aliveEnemies dg

        mapM_ (handleEnemyTurn . (^. position)) xs

handleEnemyTurn :: Coord -> State Engine ()
handleEnemyTurn c = do
        e <- get
        let dg = e ^?! dungeon

        let (messages, dg') = flip runState dg $ do
                e' <- D.popActorAt c
                case e' of
                    Just e'' -> enemyAction e''
                    Nothing  -> error "No such enemy."

        messageLog %= addMessages messages
        dungeon .= dg'

playerBumpAction :: V2 Int -> State Engine ()
playerBumpAction offset = do
        e <- get
        let dg = e ^?! dungeon

        let (result, newDungeon) = flip runState dg $ do
                e' <- D.popPlayer
                bumpAction e' offset

        case result of
            LogReturned x -> do
                messageLog %= addMessages x
                dungeon .= newDungeon
            TalkStarted tw -> put $ Talking { _talk = tw
                                            , _afterTalking = e
                                            }

playerCurrentHp :: Engine -> Int
playerCurrentHp e = E.getHp $ getPlayerEntity (e ^?! dungeon)

playerMaxHp :: Engine -> Int
playerMaxHp e = getPlayerEntity (e ^?! dungeon) ^. maxHp

playerPosition :: Engine -> Coord
playerPosition (PlayerIsExploring d _ _) = D.playerPosition d
playerPosition (Talking _ e)             = playerPosition e
playerPosition (HandlingScene _ e)       = playerPosition e

currentMapWidthAndHeight :: Engine -> V2 Int
currentMapWidthAndHeight (PlayerIsExploring d _ _) = mapWidthAndHeight d
currentMapWidthAndHeight (Talking _ e)             = currentMapWidthAndHeight e
currentMapWidthAndHeight (HandlingScene _ e)       = currentMapWidthAndHeight e

initEngine :: Engine
initEngine = HandlingScene { _scene = gameStartScene
                           , _afterFinish = initPlayerIsExploring
                           }
    where initPlayerIsExploring = PlayerIsExploring { _dungeon = initDungeon
                                                    , _messageLog = foldr (addMessage . L.message) L.emptyLog ["Welcome to a roguelike game!"]
                                                    , _isGameOver = False
                                                    }
