{-# LANGUAGE OverloadedStrings #-}

module UI.Draw
    ( drawUI
    , windowWidth
    , windowHeight
    , tileColumns
    , tileRows
    ) where
import           Control.Lens          ((&), (.~), (^.))
import           Control.Monad         (guard)
import           Coord                 (Coord)
import           Data.Array            ((!))
import           Data.Maybe            (mapMaybe)
import           Data.Text             (append, pack)
import           Dungeon               (Dungeon, actors, explored, items,
                                        mapWidthAndHeight, playerPosition,
                                        tileMap, visible)
import           Dungeon.Actor         (defence, getHp, maxHp, power,
                                        standingImagePath, walkingImagePath)
import qualified Dungeon.Actor         as A
import           Dungeon.Item          (iconImagePath)
import qualified Dungeon.Item          as I
import qualified Dungeon.Map.Tile      as MT
import           GameStatus            (GameStatus, destructHandlingScene,
                                        destructTalking, getCurrentDungeon,
                                        getItems, getPlayerActor,
                                        getSelectingIndex, isHandlingScene,
                                        isPlayerTalking, isSelectingItemToUse,
                                        isTitle, messageLogList)
import           Linear.V2             (V2 (V2), _x, _y)
import           Monomer               (CmbAlignLeft (alignLeft),
                                        CmbBgColor (bgColor),
                                        CmbHeight (height),
                                        CmbMultiline (multiline),
                                        CmbPaddingL (paddingL),
                                        CmbPaddingT (paddingT),
                                        CmbStyleBasic (styleBasic),
                                        CmbTextColor (textColor),
                                        CmbTextSize (textSize),
                                        CmbWidth (width), WidgetEnv,
                                        WidgetEvent, WidgetModel, WidgetNode,
                                        black, box_, filler, gray, hgrid,
                                        hstack, image, keystroke, label, label_,
                                        red, vgrid, vstack, zstack)
import qualified Monomer.Graphics.Lens as L
import           Scene                 (backgroundImage, elements, text)
import           Talking               (TalkWith, message, person)
import           UI.Types              (AppEvent (AppKeyboardInput))

drawUI :: WidgetEnv GameStatus AppEvent -> GameStatus -> WidgetNode GameStatus AppEvent
drawUI wenv gs
    | isPlayerTalking gs = drawTalking wenv gs
    | isHandlingScene gs = drawHandlingScene gs
    | isSelectingItemToUse gs = drawSelectingItem gs
    | isTitle gs = drawTitle
    | otherwise = drawGameMap gs

withKeyEvents :: WidgetNode s AppEvent -> WidgetNode s AppEvent
withKeyEvents =
    keystroke $ map (\x -> (x, AppKeyboardInput x))
    [ "Up"
    , "Down"
    , "Right"
    , "Left"
    , "Enter"
    , "n"
    , "l"
    , "q"
    , "g"
    , "u"
    , "Ctrl-s"
    , "Ctrl-l"
    , "Esc"
    ]

drawTalking ::  WidgetEnv GameStatus AppEvent -> GameStatus -> WidgetNode GameStatus AppEvent
drawTalking wenv e = withKeyEvents $ zstack [ drawUI wenv afterGameStatus `styleBasic` [bgColor $ gray & L.a .~ 0.5]
                                            , filler `styleBasic` [bgColor $ black & L.a .~ 0.5]
                                            , talkingWindow with
                                            ]
    where (with, afterGameStatus) = destructTalking e

drawHandlingScene :: GameStatus -> WidgetNode GameStatus AppEvent
drawHandlingScene e = withKeyEvents $ zstack [ image (s ^. backgroundImage)
                                             , label_  (text $ head $ s ^. elements) [multiline] `styleBasic` [textColor black]
                                             ]
    where (s, _) = destructHandlingScene e

drawSelectingItem :: GameStatus -> WidgetNode GameStatus AppEvent
drawSelectingItem gs = withKeyEvents $ vstack labels
    where labels = label "Which Item do you use?":map label addAsterlist
          addAsterlist = zipWith (\idx x -> if idx == getSelectingIndex gs
                                                          then "* " `append` pack (show idx) `append` " " `append`x
                                                          else pack (show idx) `append` " " `append` x
                                               ) [0..] itemNames
          itemNames = map (^. I.name) $ getItems gs

drawTitle :: WidgetNode GameStatus AppEvent
drawTitle = withKeyEvents $ vstack [ label "Gimlight" `styleBasic` [textSize 36]
                                   , label "[n] New game"
                                   , label "[l] Load the savedata"
                                   , label "[q] Quit"
                                   ]

drawGameMap :: GameStatus -> WidgetNode GameStatus AppEvent
drawGameMap gs = withKeyEvents $ vstack [ statusAndMapGrid
                                        , messageLogArea gs
                                        ] `styleBasic` [width 0]
    where statusAndMapGrid = hstack [ mapGrid gs
                                    , statusGrid gs `styleBasic` [width $ fromIntegral $ windowWidth - tileWidth * tileColumns]
                                    ]

mapGrid :: GameStatus -> WidgetNode GameStatus AppEvent
mapGrid gs = zstack (mapTiles gs:(mapItems gs ++ mapActors gs))
    `styleBasic` [ width $ fromIntegral mapDrawingWidth
                 , height $ fromIntegral mapDrawingHeight
                 ]

mapTiles :: (WidgetModel s, WidgetEvent e) => GameStatus ->  WidgetNode s e
mapTiles e = box_ [alignLeft] $ vgrid rows `styleBasic` styles
    where d = getCurrentDungeon e
          V2 bottomLeftX bottomLeftY = bottomLeftCoord d
          rows = [hgrid $ row y | y <- [bottomLeftY + tileRows - 1, bottomLeftY + tileRows - 2 .. bottomLeftY]]
          row y = [cell $ V2 x y | x <- [bottomLeftX .. bottomLeftX + tileColumns - 1]]

          isVisible c = (d ^. visible) ! c
          isExplored c = (d ^. explored) ! c

          cell c = zstack [ image $ pack $ (d ^. tileMap) ! c ^. MT.imagePath
                          , filler `styleBasic` [bgColor $ black & L.a .~ cellOpacity c]
                          ]
          cellOpacity c = case (isVisible c, isExplored c) of
                            (True, _) -> 0
                            (_, True) -> 0.5
                            _         -> 1

          styles = [ width $ fromIntegral mapDrawingWidth
                   , height $ fromIntegral mapDrawingHeight]

mapActors :: GameStatus -> [WidgetNode GameStatus AppEvent]
mapActors e = mapMaybe actorToImage $ d ^. actors
    where d = getCurrentDungeon e
          leftPadding actor = fromIntegral $ actorPositionOnDisplay actor ^. _x * tileWidth
          topPadding actor = fromIntegral $ mapDrawingHeight - (actorPositionOnDisplay actor ^. _y + 1) * tileHeight

          style actor = [paddingL $ leftPadding actor, paddingT $ topPadding actor]

          actorPositionOnDisplay actor = actor ^. A.position - bottomLeftCoord d

          isActorDrawed actor = let pos = actorPositionOnDisplay actor
                                    isVisible = (d ^. visible) ! (actor ^. A.position)
                                in V2 0 0 <= pos && pos <= topRightCoord d && isVisible

          actorToImage actor = guard (isActorDrawed actor) >> return (image (actor ^. walkingImagePath) `styleBasic` style actor)

mapItems :: GameStatus -> [WidgetNode GameStatus AppEvent]
mapItems e = mapMaybe itemToImage $ d ^. items
    where itemToImage item = guard (isItemDrawed item) >> return (image (item ^. iconImagePath) `styleBasic` style item)
          isItemDrawed item = let pos = itemPositionOnDisplay item
                                  isVisible = (d ^. visible) ! (item ^. I.position)
                                in V2 0 0 <= pos && pos <= topRightCoord d && isVisible
          d = getCurrentDungeon e
          leftPadding item = fromIntegral $ itemPositionOnDisplay item ^. _x * tileWidth
          topPadding item = fromIntegral $ mapDrawingHeight - (itemPositionOnDisplay item ^. _y + 1) * tileHeight

          style item = [paddingL $ leftPadding item, paddingT $ topPadding item]

          itemPositionOnDisplay item = item ^. I.position - bottomLeftCoord d

statusGrid :: GameStatus -> WidgetNode GameStatus AppEvent
statusGrid gs = vstack $ maybe []
    (\x -> [ label "Player"
           , label $ "HP: " `append` pack (show $ getHp x) `append` " / " `append` pack (show $ x ^. maxHp)
           , label $ "ATK: " `append` pack (show $ x ^. power)
           , label $ "DEF: " `append` pack (show $ x ^. defence)
           ]) $ getPlayerActor gs

talkingWindow :: TalkWith -> WidgetNode GameStatus AppEvent
talkingWindow tw = hstack [ image (tw ^. person . standingImagePath)
                          , window
                          ]
    where window = zstack [ image "images/talking_window.png"
                          , label (tw ^. message) `styleBasic` [textColor red, textSize 16, paddingL 50]
                          ]

messageLogArea :: (WidgetModel s, WidgetEvent e) => GameStatus -> WidgetNode s e
messageLogArea e = vstack $ fmap (\x -> label_ x [multiline]) $ take logRows $ messageLogList e

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
