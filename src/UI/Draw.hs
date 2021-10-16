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
import           Dungeon               (mapWidthAndHeight, playerPosition)
import           Dungeon.Entity        (getHp)
import qualified Dungeon.Map.Tile      as MT
import           Dungeon.Types         (Dungeon, defence, entities, explored,
                                        maxHp, position, power,
                                        standingImagePath, tileMap, visible)
import qualified Dungeon.Types         as DT
import           GameStatus            (GameStatus (HandlingScene, PlayerIsExploring, Talking, Title),
                                        getPlayerEntity, messageLogList)
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
drawUI wenv (Talking with afterGameStatus) = withKeyEvents $ zstack [ drawUI wenv afterGameStatus `styleBasic` [bgColor $ gray & L.a .~ 0.5]
                                                                , filler `styleBasic` [bgColor $ black & L.a .~ 0.5]
                                                                , talkingWindow with
                                                                ]
drawUI _ (HandlingScene s _) = withKeyEvents $ zstack [ image (s ^. backgroundImage)
                                                      , label_  (text $ head $ s ^. elements) [multiline] `styleBasic` [textColor black]
                                                      ]
drawUI _ Title = withKeyEvents $ vstack [ label "Gimlight" `styleBasic` [textSize 36]
                                        , label "[n] New game"
                                        , label "[l] Load the savedata"
                                        , label "[q] Quit"
                                        ]
drawUI _ gameStatus = withKeyEvents $ vstack [ statusAndMapGrid
                                             , messageLogArea gameStatus
                                             ] `styleBasic` [width 0]
    where statusAndMapGrid = hstack [ mapGrid gameStatus
                                    , statusGrid gameStatus `styleBasic` [width $ fromIntegral $ windowWidth - tileWidth * tileColumns]
                                    ]

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
    , "Ctrl-s"
    , "Ctrl-l"
    ]

mapGrid :: (WidgetModel s, WidgetEvent e) => GameStatus -> WidgetNode s e
mapGrid gameStatus = zstack (mapTiles gameStatus:mapEntities gameStatus) `styleBasic` [ width $ fromIntegral mapDrawingWidth
                                                                          , height $ fromIntegral mapDrawingHeight
                                                                          ]

mapTiles :: (WidgetModel s, WidgetEvent e) => GameStatus ->  WidgetNode s e
mapTiles (PlayerIsExploring d _ _ _) = box_ [alignLeft] $ vgrid rows `styleBasic` styles
    where V2 bottomLeftX bottomLeftY = bottomLeftCoord d
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
mapTiles _ = undefined

mapEntities :: (WidgetModel s, WidgetEvent e) => GameStatus -> [WidgetNode s e]
mapEntities (PlayerIsExploring d _ _ _) = mapMaybe entityToImage $ d ^. entities
    where leftPadding e = fromIntegral $ entityPositionOnDisplay e ^. _x * tileWidth
          topPadding e = fromIntegral $ mapDrawingHeight - (entityPositionOnDisplay e ^. _y + 1) * tileHeight

          style e = [paddingL $ leftPadding e, paddingT $ topPadding e]

          entityPositionOnDisplay e = e ^. position - bottomLeftCoord d

          isEntityDrawed e = let pos = entityPositionOnDisplay e
                                 isVisible = (d ^. visible) ! (e ^. position)
                             in V2 0 0 <= pos && pos <= topRightCoord d && isVisible

          entityToImage e = guard (isEntityDrawed e) >> return (image (e ^. DT.walkingImagePath) `styleBasic` style e)
mapEntities _                         = undefined

statusGrid :: GameStatus -> WidgetNode GameStatus AppEvent
statusGrid gs = vstack $ maybe []
    (\x -> [ label "Player"
           , label $ "HP: " `append` pack (show $ getHp x) `append` " / " `append` pack (show $ x ^. maxHp)
           , label $ "ATK: " `append` pack (show $ x ^. power)
           , label $ "DEF: " `append` pack (show $ x ^. defence)
           ]) $ getPlayerEntity gs

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
