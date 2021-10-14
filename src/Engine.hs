{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Engine where

import           Control.Lens                   (makeLenses, (%=), (%~), (&),
                                                 (.=), (.~), (^.), (^?!))
import           Control.Monad.Trans.State      (State, get)
import           Control.Monad.Trans.State.Lazy (put, runState)
import           Coord                          (Coord)
import           Data.Binary                    (Binary)
import           Data.List                      (find, findIndex)
import           Dungeon                        (Dungeon, aliveEnemies,
                                                 getPlayerEntity, initDungeon,
                                                 mapWidthAndHeight)
import qualified Dungeon                        as D
import qualified Dungeon.Entity                 as E
import           Dungeon.Entity.Behavior        (BumpResult (..), bumpAction,
                                                 enemyAction)
import           Dungeon.Predefined.BatsCave    (batsDungeon)
import           Dungeon.Predefined.GlobalMap   (globalMap)
import qualified Dungeon.Turn                   as DT
import           Dungeon.Types                  (entities, isGlobalMap, maxHp,
                                                 position, positionOnGlobalMap)
import           GHC.Generics                   (Generic)
import           Linear.V2                      (V2)
import           Log                            (MessageLog, addMessage,
                                                 addMessages)
import qualified Log                            as L
import           Scene                          (Scene, gameStartScene)
import           System.Random                  (getStdGen)
import           Talking                        (TalkWith)

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
        handleEnemyTurns

        e <- get
        let dg = e ^?! currentDungeon

        let (status, newD) = runState D.completeThisTurn dg

        isGameOver .= (status == DT.PlayerKilled)

        currentDungeon .= newD

handleEnemyTurns :: State Engine ()
handleEnemyTurns = do
        e <- get
        let dg = e ^?! currentDungeon

        let xs = aliveEnemies dg

        mapM_ (handleEnemyTurn . (^. position)) xs

handleEnemyTurn :: Coord -> State Engine ()
handleEnemyTurn c = do
        e <- get
        let dg = e ^?! currentDungeon

        let (result, dg') = flip runState dg $ do
                e' <- D.popActorAt c
                case e' of
                    Just e'' -> enemyAction e''
                    Nothing  -> error "No such enemy."

        messageLog %= addMessages (case result of
                                      LogReturned m -> m
                                      Ok            -> []
                                      _             -> error "unreachable")
        currentDungeon .= dg'

playerBumpAction :: V2 Int -> State Engine ()
playerBumpAction offset = do
        e <- get
        let dg = e ^?! currentDungeon

        let (result, newDungeon) = flip runState dg $ do
                e' <- D.popPlayer
                bumpAction e' offset

        case result of
            LogReturned x -> do
                messageLog %= addMessages x
                currentDungeon .= newDungeon
            TalkStarted tw -> put $ Talking { _talk = tw
                                            , _afterTalking = e
                                            }
            ExitToGlobalMap p -> do
                otherDungeons %= (:) newDungeon
                let newPosition = newDungeon ^. positionOnGlobalMap
                let g = find (^. isGlobalMap) (e ^?! otherDungeons)
                let newPlayer = p & position .~ case newPosition of
                                                    Just pos -> pos
                                                    Nothing  -> error "whooops."
                currentDungeon .= case g of
                                      Just g' -> g' & entities %~ (:) newPlayer
                                      Nothing -> error "whoops."
            Ok -> currentDungeon .= newDungeon



playerCurrentHp :: Engine -> Int
playerCurrentHp e = E.getHp $ getPlayerEntity (e ^?! currentDungeon)

playerMaxHp :: Engine -> Int
playerMaxHp e = getPlayerEntity (e ^?! currentDungeon) ^. maxHp

playerPosition :: Engine -> Coord
playerPosition (PlayerIsExploring d _ _ _) = D.playerPosition d
playerPosition (Talking _ e)               = playerPosition e
playerPosition (HandlingScene _ e)         = playerPosition e
playerPosition Title                       = error "unreachable."

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
