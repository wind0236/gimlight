module Game
    ( Game
    , isPlayerExploring
    , isPlayerTalking
    , isHandlingScene
    , isSelectingItemToUse
    , isTitle
    , isGameOver
    , isSelectingLocale
    , handlePlayerMoving
    , handlePlayerPickingUp
    , handlePlayerSelectingItemToUse
    , handlePlayerEnteringTown
    , handlePlayerConsumingItem
    , saveStatus
    , loadStatus
    , finishTalking
    , nextSceneElementOrFinish
    , selectPrevItem
    , selectNextItem
    , getSelectingIndex
    , getItems
    , finishSelecting
    , destructTalking
    , destructHandlingScene
    , getCurrentDungeon
    , getPlayerActor
    , getMessageLog
    , getLocalizedText
    , setLocale
    , startNewGame
    , afterBooting
    ) where

import           Control.Monad.Trans.State (execState)
import           Data.Text                 (Text)
import           Dungeon                   (Dungeon)
import           Dungeon.Actor             (Actor)
import           Dungeon.Item              (Item)
import           Game.Config               (Config, Language, getLocale,
                                            readConfigOrDefault, writeConfig)
import qualified Game.Config               as C
import           Game.Status               (GameStatus, selectingLocale, title)
import qualified Game.Status               as GS
import qualified Game.Status.Player        as GSP
import           Linear.V2                 (V2)
import           Localization              (MultilingualText)
import qualified Localization              as L
import           Log                       (MessageLog)
import qualified Save
import           Scene                     (Scene)
import           Talking                   (TalkWith)

data Game = Game
          { status :: GameStatus
          , config :: Config
          } deriving (Eq, Show)

isPlayerExploring :: Game -> Bool
isPlayerExploring Game { status = s } = GS.isPlayerExploring s

isPlayerTalking :: Game -> Bool
isPlayerTalking Game { status = s } = GS.isPlayerTalking s

isHandlingScene :: Game -> Bool
isHandlingScene Game { status = s } = GS.isHandlingScene s

isSelectingItemToUse :: Game -> Bool
isSelectingItemToUse Game { status = s } = GS.isSelectingItemToUse s

isTitle :: Game -> Bool
isTitle Game { status = s } = GS.isTitle s

isGameOver :: Game -> Bool
isGameOver Game { status = s } = GS.isGameOver s

isSelectingLocale :: Game -> Bool
isSelectingLocale Game { status = s } = GS.isSelectingLocale s

handlePlayerMoving :: V2 Int -> Game -> Game
handlePlayerMoving offset g@Game { status = s } =
    g { status = flip execState s $ GSP.handlePlayerMoving offset }

handlePlayerSelectingItemToUse :: Game -> Game
handlePlayerSelectingItemToUse g@Game { status = s } =
    g { status = GSP.handlePlayerSelectingItemToUse s }

handlePlayerEnteringTown :: Game -> Game
handlePlayerEnteringTown g@Game { status = s } =
    g { status = GS.enterTownAtPlayerPosition s }

handlePlayerPickingUp :: Game -> Game
handlePlayerPickingUp g@Game { status = s } =
    g { status = execState GSP.handlePlayerPickingUp s }

handlePlayerConsumingItem :: Game -> Game
handlePlayerConsumingItem g@Game { status = s } =
    g { status = execState GSP.handlePlayerConsumeItem s }

saveStatus :: Game -> IO ()
saveStatus Game { status = s } = Save.save s

loadStatus :: Game -> IO Game
loadStatus g = do
    s <-Save.load

    return g { status = s }

destructTalking :: Game -> (TalkWith, Game)
destructTalking g@Game{ status = s } = (tw, g { status = afterStatus })
    where (tw, afterStatus) = GS.destructTalking s

destructHandlingScene :: Game -> (Scene, Game)
destructHandlingScene g@Game { status = s } = (sc, g { status = afterScene })
    where (sc, afterScene) = GS.destructHandlingScene s

finishTalking :: Game -> Game
finishTalking g@Game { status = s } =
    g { status = GS.finishTalking s }

nextSceneElementOrFinish :: Game -> Game
nextSceneElementOrFinish g@Game { status = s } =
    g { status = GS.nextSceneElementOrFinish s }

selectPrevItem :: Game -> Game
selectPrevItem g@Game { status = s } =
    g { status = GS.selectPrevItem s }

selectNextItem :: Game -> Game
selectNextItem g@Game { status = s } =
    g { status = GS.selectNextItem s }

getSelectingIndex :: Game -> Int
getSelectingIndex Game { status = s } = GS.getSelectingIndex s

getItems :: Game -> [Item]
getItems Game { status = s } = GS.getItems s

finishSelecting :: Game -> Game
finishSelecting g@Game { status = s } =
    g { status = GS.finishSelecting s }

startNewGame :: Game -> IO Game
startNewGame Game { config = c } = do
    st <- GS.newGameStatus

    return Game { status = st
                , config = c
                }

getCurrentDungeon :: Game -> Dungeon
getCurrentDungeon Game { status = s } = GS.getCurrentDungeon s

getPlayerActor :: Game -> Maybe Actor
getPlayerActor Game { status = s } = GS.getPlayerActor s

getMessageLog :: Game -> MessageLog
getMessageLog Game { status = s } = GS.messageLogList s

getLocalizedText :: Game -> MultilingualText -> Text
getLocalizedText Game { config = c } = L.getLocalizedText c

setLocale :: Language -> Game -> IO Game
setLocale l g@Game { config = c } = do
    let newConfig = C.setLocale l c

    writeConfig newConfig

    return $ g { status = title
               , config = newConfig
               }

afterBooting :: IO Game
afterBooting = do
    initConfig <- readConfigOrDefault

    let initStatus = case getLocale initConfig of
                         Just _  -> title
                         Nothing -> selectingLocale

    return Game { status = initStatus
                , config = initConfig
                }
