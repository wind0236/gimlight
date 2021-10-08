module UI.Draw
    ( drawUI
    ) where
import           Control.Lens          ((&), (.~), (^.))
import           Control.Monad         (guard)
import           Coord                 (Coord)
import           Data.Array            ((!))
import           Data.Bifunctor        (second)
import           Data.Maybe            (mapMaybe)
import           Data.Text             (pack)
import           Dungeon               (mapWidthAndHeight, playerPosition)
import qualified Dungeon.Map.Tile      as MT
import           Dungeon.Types         (Dungeon, entities, position,
                                        standingImagePath, tileMap)
import qualified Dungeon.Types         as DT
import           Engine                (Engine (PlayerIsExploring, Talking))
import           Linear.V2             (V2 (V2), _x, _y)
import           Monomer               (CmbAlignLeft (alignLeft),
                                        CmbBgColor (bgColor),
                                        CmbHeight (height),
                                        CmbPaddingL (paddingL),
                                        CmbPaddingT (paddingT),
                                        CmbStyleBasic (styleBasic),
                                        CmbWidth (width), WidgetEnv,
                                        WidgetEvent, WidgetModel, WidgetNode,
                                        black, box_, filler, gray, hgrid, image,
                                        keyDown, keyLeft, keyRight, keyUp,
                                        keystroke, label, vgrid, vstack, zstack)
import qualified Monomer.Graphics.Lens as L
import           Talking               (TalkWith, person)
import           UI.Types              (AppEvent (AppKeyboardInput))

drawUI :: WidgetEnv Engine AppEvent -> Engine -> WidgetNode Engine AppEvent
drawUI wenv (Talking with afterEngine) = withKeyEvents $ zstack [ drawUI wenv afterEngine `styleBasic` [bgColor $ gray & L.a .~ 0.5]
                                                                , filler `styleBasic` [bgColor $ black & L.a .~ 0.5]
                                                                , partner with
                                                                ]
drawUI _ engine = withKeyEvents $ vstack [ mapGrid engine
                                         , label $ pack "多分ここにログが表示される．"
                                         ] `styleBasic` [width 0]

withKeyEvents :: WidgetNode s AppEvent -> WidgetNode s AppEvent
withKeyEvents =
    keystroke $ map (second AppKeyboardInput)
    [ (pack "Up", keyUp)
    , (pack "Down", keyDown)
    , (pack "Right", keyRight)
    , (pack "Left", keyLeft)
    ]

mapGrid :: (WidgetModel s, WidgetEvent e) => Engine -> WidgetNode s e
mapGrid engine = zstack (mapTiles engine:mapEntities engine) `styleBasic` [ width $ fromIntegral mapDrawingWidth
                                                                          , height $ fromIntegral mapDrawingHeight
                                                                          ]

mapTiles :: (WidgetModel s, WidgetEvent e) => Engine ->  WidgetNode s e
mapTiles (PlayerIsExploring d _ _) = box_ [alignLeft] $ vgrid rows `styleBasic` styles
    where V2 bottomLeftX bottomLeftY = bottomLeftCoord d
          rows = [hgrid $ row y | y <- [bottomLeftY + tileRows - 1, bottomLeftY + tileRows - 2 .. bottomLeftY]]
          row y = [cell (x, y) | x <- [bottomLeftX .. bottomLeftX + tileColumns - 1]]
          cell c = image $ pack $ (d ^. tileMap) ! c ^. MT.imagePath
          styles = [ width $ fromIntegral mapDrawingWidth
                   , height $ fromIntegral mapDrawingHeight]
mapTiles _ = undefined

mapEntities :: (WidgetModel s, WidgetEvent e) => Engine -> [WidgetNode s e]
mapEntities (PlayerIsExploring d _ _) = mapMaybe entityToImage $ d ^. entities
    where leftPadding e = fromIntegral $ entityPositionOnDisplay e ^. _x * tileWidth
          topPadding e = fromIntegral $ mapDrawingHeight - (entityPositionOnDisplay e ^. _y + 1) * tileHeight

          style e = [paddingL $ leftPadding e, paddingT $ topPadding e]

          entityPositionOnDisplay e = e ^. position - bottomLeftCoord d

          isEntityDrawed e = let pos = entityPositionOnDisplay e
                             in V2 0 0 <= pos && pos <= topRightCoord d

          entityToImage e = guard (isEntityDrawed e) >> return (image (pack $ e ^. DT.walkingImagePath) `styleBasic` style e)
mapEntities _                         = undefined

partner :: TalkWith -> WidgetNode Engine AppEvent
partner tw = zstack [ image (pack $ tw ^. (person . standingImagePath)) `styleBasic` style]
    where style = []

topRightCoord :: Dungeon -> Coord
topRightCoord d = bottomLeftCoord d + mapWidthAndHeight d - V2 1 1

bottomLeftCoord :: Dungeon -> Coord
bottomLeftCoord d = V2 x y
    where V2 unadjustedX unadjestedY = playerPosition d - V2 (tileColumns `div` 2) (tileRows `div` 2)
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
tileColumns = 25
tileRows = 19

-- standingImageWidth, standingImageHeight :: Int
-- standingImageWidth = 300
-- standingImageHeight = 600
