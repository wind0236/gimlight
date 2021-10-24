{-# LANGUAGE LambdaCase #-}
module Game.Status.Player
    ( playerBumpAction
    , handlePlayerMoving
    , handlePlayerPickingUp
    , handlePlayerSelectingItemToUse
    , handlePlayerConsumeItem
    ) where
import           Control.Lens                   ((^.))
import           Control.Monad                  (unless, when)
import           Control.Monad.Trans.State      (State, get, put, state)
import           Dungeon                        (isTown)
import           Dungeon.Actor                  (Actor, isMonster, talkMessage)
import qualified Dungeon.Actor                  as A
import           Dungeon.Actor.Actions          (Action, consumeAction,
                                                 meleeAction, moveAction,
                                                 pickUpAction)
import           Game.Status                    (GameStatus (Exploring, SelectingItemToUse),
                                                 actorAt, completeThisTurn,
                                                 finishSelecting,
                                                 getCurrentDungeon,
                                                 getPlayerActor,
                                                 getSelectingIndex, isGameOver,
                                                 isPlayerExploring,
                                                 isPositionInDungeon,
                                                 playerPosition, talking)
import qualified Game.Status.Exploring          as GSE
import           Game.Status.SelectingItemToUse (selectingItemToUseHandler)
import           Linear.V2                      (V2)
import           Talking                        (talkWith)

playerBumpAction :: V2 Int -> State GameStatus Bool
playerBumpAction offset = do
    gameStatus <- get

    let destination = case playerPosition gameStatus of
                          Just p  -> p + offset
                          Nothing -> error "The player is dead."

    case actorAt destination gameStatus of
        Just actorAtDestination -> meleeOrTalk offset actorAtDestination
        Nothing                 -> moveOrExitMap offset

meleeOrTalk :: V2 Int -> Actor -> State GameStatus Bool
meleeOrTalk offset target = do
    gameStatus <- get

    if isMonster target
        then doAction $ meleeAction offset
        else do
            put $ talking (talkWith target $ target ^. talkMessage) gameStatus
            return True

moveOrExitMap :: V2 Int -> State GameStatus Bool
moveOrExitMap offset = do
    gameStatus <- get

    let destination = case playerPosition gameStatus of
                          Just p  -> p + offset
                          Nothing -> error "The player is dead."

    if isPositionInDungeon destination gameStatus || not (isTown (getCurrentDungeon gameStatus))
        then doAction $ moveAction offset
        else do
            exitDungeon
            return True

exitDungeon :: State GameStatus ()
exitDungeon = state $ \case
    Exploring eh -> case GSE.exitDungeon eh of
                        Just newEh -> ((), Exploring newEh)
                        Nothing    -> error "Failed to exit from the dungeon."
    _ -> undefined

handlePlayerMoving :: V2 Int -> State GameStatus ()
handlePlayerMoving offset = do
    eng <- get
    let finished = isGameOver eng
    unless finished $ do
        success <- playerBumpAction offset

        when success $ do
            eng' <- get

            when (isPlayerExploring eng') completeThisTurn

handlePlayerPickingUp :: State GameStatus ()
handlePlayerPickingUp = do
    eng <- get
    let finished = isGameOver eng
    unless finished $ do
        success <- doAction pickUpAction

        when success $ do
            eng' <- get
            when (isPlayerExploring eng') completeThisTurn

handlePlayerSelectingItemToUse :: GameStatus -> GameStatus
handlePlayerSelectingItemToUse e@(Exploring eh) =
    SelectingItemToUse $ selectingItemToUseHandler xs eh
    where xs = A.getItems p
          p = case getPlayerActor e of
                Just x  -> x
                Nothing -> error "Player is dead."
handlePlayerSelectingItemToUse _ = undefined

handlePlayerConsumeItem :: State GameStatus ()
handlePlayerConsumeItem = do
    gs <- get

    case getSelectingIndex gs of
        Just n -> do
            put $ finishSelecting gs

            success <- doAction $ consumeAction n

            when success $ do
                completeThisTurn
        Nothing -> return ()

doAction :: Action -> State GameStatus Bool
doAction action = state $ \case
    Exploring eh -> (\(newEh, s) -> (s, Exploring newEh)) $ GSE.doAction action eh
    _            -> undefined
