module Game.Status.Player
    ( playerBumpAction
    , handlePlayerMoving
    , handlePlayerPickingUp
    , handlePlayerSelectingItemToUse
    , handlePlayerConsumeItem
    ) where
import           Control.Lens              ((%~), (&), (.=), (.~), (^.), (^?!))
import           Control.Monad             (unless, when)
import           Control.Monad.Trans.State (State, get, put, runState)
import           Data.Foldable             (find)
import           Dungeon                   (actors, isGlobalMap, isTown,
                                            popPlayer, positionOnGlobalMap)
import           Dungeon.Actor             (Actor, isMonster, position,
                                            talkMessage)
import qualified Dungeon.Actor             as A
import           Dungeon.Actor.Actions     (Action, consumeAction, meleeAction,
                                            moveAction, pickUpAction)
import           Game.Status               (GameStatus, actorAt, addMessages,
                                            completeThisTurn, currentDungeon,
                                            finishSelecting, getCurrentDungeon,
                                            getOtherDungeons, getPlayerActor,
                                            getSelectingIndex, isGameOver,
                                            isPlayerExploring,
                                            isPositionInDungeon,
                                            isSelectingListEmpty,
                                            playerPosition,
                                            pushDungeonAsOtherDungeons,
                                            selectingItemToUse, talking)
import           Linear.V2                 (V2)
import           Talking                   (talkWith)

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
exitDungeon = do
    gs <- get

    let (p, currentDungeon') = runState popPlayer (getCurrentDungeon gs)

    pushDungeonAsOtherDungeons currentDungeon'

    let g = find isGlobalMap $ getOtherDungeons gs
        newPosition = currentDungeon' ^. positionOnGlobalMap
        newPlayer = p & position .~ case newPosition of
            Just pos -> pos
            Nothing  -> error "The new position is not specified."
    currentDungeon .= case g of
        Just g' -> g' & actors %~ (:) newPlayer
        Nothing -> error "Global map not found."
    return ()


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
handlePlayerSelectingItemToUse e =
    selectingItemToUse xs e
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

doAction :: Action -> State GameStatus Bool
doAction action = do
    gs <- get

    let ((msg, success), currentDungeon') = flip runState (gs ^?! currentDungeon) $ do
            p <- popPlayer
            action p
    addMessages msg
    currentDungeon .= currentDungeon'
    return success
