{-# LANGUAGE OverloadedStrings #-}

module UI.Draw
    ( drawUI
    , windowWidth
    , windowHeight
    , tileColumns
    , tileRows
    ) where
import           Control.Lens                   ((&), (.~), (^.))
import           Control.Monad                  (guard)
import           Coord                          (Coord)
import           Data.Array                     ((!))
import           Data.Maybe                     (mapMaybe)
import           Data.Text                      (append)
import           Dungeon                        (Dungeon, actors, explored,
                                                 items, mapWidthAndHeight,
                                                 playerPosition, tileMap,
                                                 visible)
import           Dungeon.Actor                  (getDefence, getHp, getMaxHp,
                                                 getPower, standingImagePath,
                                                 walkingImagePath)
import qualified Dungeon.Actor                  as A
import           Dungeon.Item                   (iconImagePath)
import qualified Dungeon.Item                   as I
import qualified Dungeon.Map.Tile               as MT
import           Game                           (Game (Game, config, status))
import           Game.Status                    (GameStatus (Exploring, GameOver, HandlingScene, SelectingItemToUse, SelectingLocale, Talking, Title))
import           Game.Status.Exploring          (getCurrentDungeon,
                                                 getMessageLog, getPlayerActor)
import qualified Game.Status.Scene              as GSS
import           Game.Status.SelectingItemToUse (finishSelecting, getItems,
                                                 getSelectingIndex)
import qualified Game.Status.Talking            as GST
import           Linear.V2                      (V2 (V2), _x, _y)
import           Localization                   (getLocalizedText,
                                                 multilingualText)
import           Monomer                        (CmbAlignLeft (alignLeft),
                                                 CmbBgColor (bgColor),
                                                 CmbHeight (height),
                                                 CmbMultiline (multiline),
                                                 CmbPaddingL (paddingL),
                                                 CmbPaddingT (paddingT),
                                                 CmbStyleBasic (styleBasic),
                                                 CmbTextColor (textColor),
                                                 CmbTextSize (textSize),
                                                 CmbWidth (width), WidgetEnv,
                                                 WidgetNode, black, box_,
                                                 filler, gray, hgrid, hstack,
                                                 image, keystroke, label,
                                                 label_, red, vgrid, vstack,
                                                 zstack)
import qualified Monomer.Graphics.Lens          as L
import           Scene                          (backgroundImage, elements,
                                                 text)
import           Talking                        (TalkWith, message, person)
import           TextShow                       (TextShow (showt))
import           UI.Types                       (AppEvent (AppKeyboardInput))

type GameWidgetEnv = WidgetEnv Game AppEvent
type GameWidgetNode = WidgetNode Game AppEvent

drawUI :: GameWidgetEnv -> Game -> GameWidgetNode
drawUI wenv gs@Game { status = s } =
    case s of
        Exploring _          -> drawGameMap gs
        Talking _            -> drawTalking wenv gs
        HandlingScene _      -> drawHandlingScene gs
        SelectingItemToUse _ -> drawSelectingItem gs
        Title                -> drawTitle gs
        GameOver             -> drawGameOver
        SelectingLocale      -> drawSelectingLanguage

withKeyEvents :: WidgetNode s AppEvent -> WidgetNode s AppEvent
withKeyEvents =
    keystroke $ map (\x -> (x, AppKeyboardInput x))
    [ "Up"
    , "Down"
    , "Right"
    , "Left"
    , "Enter"
    , "Shift-."
    , "Shift-,"
    , "n"
    , "l"
    , "q"
    , "g"
    , "u"
    , "e"
    , "j"
    , "Ctrl-s"
    , "Ctrl-l"
    , "Esc"
    ]

drawTalking ::  GameWidgetEnv -> Game -> GameWidgetNode
drawTalking wenv e@Game { status = Talking th } =
    withKeyEvents $ zstack [ drawUI wenv (e { status = Exploring afterGameStatus }) `styleBasic` [bgColor $ gray & L.a .~ 0.5]
                           , filler `styleBasic` [bgColor $ black & L.a .~ 0.5]
                           , talkingWindow e with
                           ]
    where (with, afterGameStatus) = GST.destructHandler th
drawTalking _ _ = error "We are not handling a talk event."

drawHandlingScene :: Game -> GameWidgetNode
drawHandlingScene Game { status = HandlingScene sh, config = c } =
    withKeyEvents $ zstack [ image (s ^. backgroundImage)
                           , label_  (getLocalizedText c $ text $ head $ s ^. elements) [multiline] `styleBasic` [textColor black]
                           ]
    where (s, _) = GSS.destructHandler sh
drawHandlingScene _ = error "We are not handling a scene."

drawSelectingItem :: Game -> GameWidgetNode
drawSelectingItem Game { status = SelectingItemToUse sh, config = c } = withKeyEvents $ vstack labels
    where labels = label topLabel:map label addAsterlist
          addAsterlist = zipWith (\idx x -> if Just idx == getSelectingIndex sh
                                                then "* " `append` showt idx `append` " " `append`x
                                                else showt idx `append` " " `append` x
                                               ) [0..] $ map (getLocalizedText c) itemNames
          itemNames = map (^. I.name) $ getItems sh
          topLabel = getLocalizedText c $ multilingualText "Which Item do you use?" "どのアイテムを使う？"
drawSelectingItem _ = error "We are not selecting an item."

drawSelectingLanguage :: GameWidgetNode
drawSelectingLanguage = withKeyEvents $ vstack [ label "Choose your language. / 言語を選択してください．"
                                               , label "[e] English"
                                               , label "[j] 日本語"
                                               ]

drawTitle :: Game -> GameWidgetNode
drawTitle Game { config = c } = withKeyEvents $ vstack [ label "Gimlight" `styleBasic` [textSize 36]
                                     , label $ "[n] " `append` getLocalizedText c newGame
                                     , label $ "[l] " `append` getLocalizedText c loadGame
                                     , label $ "[q] " `append` getLocalizedText c quitGame
                                     ]
    where newGame = multilingualText "New game" "新しく始める"
          loadGame = multilingualText " Load the savedata" "セーブデータを読み込む"
          quitGame = multilingualText "Quit" "終了する"

drawGameOver :: GameWidgetNode
drawGameOver = vstack [label "Game Over" `styleBasic` [textSize 72]]

drawGameMap :: Game -> GameWidgetNode
drawGameMap gs = withKeyEvents $ vstack [ statusAndMapGrid
                                        , messageLogArea gs
                                        ]
    where statusAndMapGrid = hstack [ mapGrid gs
                                    , statusGrid gs `styleBasic` [width $ fromIntegral $ windowWidth - tileWidth * tileColumns]
                                    ]

mapGrid :: Game -> GameWidgetNode
mapGrid gs = zstack (mapTiles gs:(mapItems gs ++ mapActors gs))
    `styleBasic` [ width $ fromIntegral mapDrawingWidth
                 , height $ fromIntegral mapDrawingHeight
                 ]

mapTiles :: Game ->  GameWidgetNode
mapTiles Game { status = s } = box_ [alignLeft] $ vgrid rows `styleBasic` styles
    where d = getCurrentDungeon $ eh s
          V2 bottomLeftX bottomLeftY = bottomLeftCoord d
          rows = [hgrid $ row y | y <- [bottomLeftY + tileRows - 1, bottomLeftY + tileRows - 2 .. bottomLeftY]]
          row y = [cell $ V2 x y | x <- [bottomLeftX .. bottomLeftX + tileColumns - 1]]

          isVisible c = (d ^. visible) ! c
          isExplored c = (d ^. explored) ! c

          cell c = zstack [ image $ (d ^. tileMap) ! c ^. MT.imagePath
                          , filler `styleBasic` [bgColor $ black & L.a .~ cellOpacity c]
                          ]
          cellOpacity c = case (isVisible c, isExplored c) of
                            (True, _) -> 0
                            (_, True) -> 0.5
                            _         -> 1

          styles = [ width $ fromIntegral mapDrawingWidth
                   , height $ fromIntegral mapDrawingHeight]
          eh st = case st of
                      Exploring e      -> e
                      Talking th       -> snd $ GST.destructHandler th
                      HandlingScene sh -> snd $ GSS.destructHandler sh
                      _                -> error "unreachable."

mapActors :: Game -> [GameWidgetNode]
mapActors Game { status = s } = mapMaybe actorToImage $ d ^. actors
    where d = getCurrentDungeon $ eh s
          leftPadding actor = fromIntegral $ actorPositionOnDisplay actor ^. _x * tileWidth
          topPadding actor = fromIntegral $ mapDrawingHeight - (actorPositionOnDisplay actor ^. _y + 1) * tileHeight

          style actor = [paddingL $ leftPadding actor, paddingT $ topPadding actor]

          actorPositionOnDisplay actor = actor ^. A.position - bottomLeftCoord d

          isActorDrawed actor = let pos = actorPositionOnDisplay actor
                                    isVisible = (d ^. visible) ! (actor ^. A.position)
                                in V2 0 0 <= pos && pos <= topRightCoord d && isVisible

          actorToImage actor = guard (isActorDrawed actor) >> return (image (actor ^. walkingImagePath) `styleBasic` style actor)
          eh st = case st of
                      Exploring e      -> e
                      Talking th       -> snd $ GST.destructHandler th
                      HandlingScene sh -> snd $ GSS.destructHandler sh
                      _                -> error "unreachable."

mapItems :: Game -> [GameWidgetNode]
mapItems Game { status = s } = mapMaybe itemToImage $ d ^. items
    where itemToImage item = guard (isItemDrawed item) >> return (image (item ^. iconImagePath) `styleBasic` style item)
          isItemDrawed item = let pos = itemPositionOnDisplay item
                                  isVisible = (d ^. visible) ! (item ^. I.position)
                                in V2 0 0 <= pos && pos <= topRightCoord d && isVisible
          d = getCurrentDungeon $ eh s
          leftPadding item = fromIntegral $ itemPositionOnDisplay item ^. _x * tileWidth
          topPadding item = fromIntegral $ mapDrawingHeight - (itemPositionOnDisplay item ^. _y + 1) * tileHeight

          style item = [paddingL $ leftPadding item, paddingT $ topPadding item]

          itemPositionOnDisplay item = item ^. I.position - bottomLeftCoord d
          eh st = case st of
                      Exploring e      -> e
                      Talking th       -> snd $ GST.destructHandler th
                      HandlingScene sh -> snd $ GSS.destructHandler sh
                      _                -> error "unreachable."

statusGrid :: Game -> GameWidgetNode
statusGrid Game { status = s, config = c } = vstack $ maybe []
    (\x -> [ label "Player"
           , label $ "HP: " `append` showt (getHp x) `append` " / " `append` showt (getMaxHp x)
           , label $ atk `append` showt (getPower x)
           , label $ def `append` showt (getDefence x)
           ]) $ player s
    where atk = getLocalizedText c $ multilingualText "ATK: " "攻撃: "
          def = getLocalizedText c $ multilingualText "DEF: " "防御: "
          player st = case st of
                       Exploring eh -> getPlayerActor eh
                       Talking th   -> getPlayerActor $ snd $ GST.destructHandler th
                       HandlingScene sh -> getPlayerActor $ snd $ GSS.destructHandler sh
                       SelectingItemToUse i -> player $ Exploring $ finishSelecting i
                       _ -> error "No player entity."

talkingWindow :: Game -> TalkWith -> GameWidgetNode
talkingWindow Game { config = c } tw = hstack [ image (tw ^. person . standingImagePath)
                            , window
                            ]
    where window = zstack [ image "images/talking_window.png"
                          , label (getLocalizedText c (tw ^. message)) `styleBasic` [textColor red, textSize 16, paddingL 50]
                          ]

messageLogArea :: Game -> GameWidgetNode
messageLogArea Game { status = s, config = c } =
    vstack $ fmap (\x -> label_ (getLocalizedText c x) [multiline] ) $ take logRows $ ls s
    where ls st = case st of
                   Exploring eh     -> getMessageLog eh
                   Talking th       -> getMessageLog $ snd $ GST.destructHandler th
                   HandlingScene sh -> getMessageLog $ snd $ GSS.destructHandler sh
                   _                -> error "unable to print logs."

topRightCoord :: Dungeon -> Coord
topRightCoord d = bottomLeftCoord d + mapWidthAndHeight d - V2 1 1

bottomLeftCoord :: Dungeon -> Coord
bottomLeftCoord d = V2 x y
    where V2 unadjustedX unadjestedY = maybe (V2 0 0) (\pos -> pos - V2 (tileColumns `div` 2) (tileRows `div` 2)) (playerPosition d)
          V2 maxX maxY = mapWidthAndHeight d - V2 tileColumns tileRows
          x = max 0 $ min maxX unadjustedX
          y = max 0 $ min maxY unadjestedY

mapDrawingWidth, mapDrawingHeight :: Int
mapDrawingWidth = tileWidth * tileColumns
mapDrawingHeight = tileHeight * tileRows

tileWidth, tileHeight :: Int
tileWidth = 48
tileHeight = 48

tileColumns, tileRows :: Int
tileColumns = 23
tileRows = 13

logRows :: Int
logRows = 5

windowWidth, windowHeight :: Int
windowWidth = 1280
windowHeight = 720
