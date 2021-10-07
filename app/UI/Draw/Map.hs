module UI.Draw.Map
    ( mapGrid
    ) where
import           Control.Lens     ((^.))
import           Data.Array       ((!))
import           Data.Text        (pack)
import qualified Dungeon.Map.Tile as MT
import           Dungeon.Types    (entities, position, tileMap)
import qualified Dungeon.Types    as DT
import           Engine           (Engine (PlayerIsExploring))
import           Linear.V2        (_x, _y)
import           Monomer          (CmbAlignLeft (alignLeft), CmbHeight (height),
                                   CmbPaddingL (paddingL),
                                   CmbPaddingT (paddingT),
                                   CmbStyleBasic (styleBasic), CmbWidth (width),
                                   WidgetEvent, WidgetModel, WidgetNode, box_,
                                   hgrid, image, vgrid, zstack)

mapGrid :: (WidgetModel s, WidgetEvent e) => Engine -> WidgetNode s e
mapGrid engine = zstack (mapTiles engine:mapEntities engine) `styleBasic` [ width $ fromIntegral mapDrawingWidth
                                                                          , height $ fromIntegral mapDrawingHeight
                                                                          ]

mapTiles :: (WidgetModel s, WidgetEvent e) => Engine ->  WidgetNode s e
mapTiles (PlayerIsExploring d _ _) = box_ [alignLeft] $ vgrid rows `styleBasic` styles
    where rows = [hgrid $ row y | y <- [tileRows - 1, tileRows - 2 .. 0]]
          row y = [cell (x, y) | x <- [0 .. tileColumns - 1]]
          cell c = image $ pack $ ((d ^. tileMap) ! c) ^. MT.imagePath
          styles = [ width $ fromIntegral mapDrawingWidth
                   , height $ fromIntegral mapDrawingHeight]
mapTiles _ = undefined

mapEntities :: (WidgetModel s, WidgetEvent e) => Engine -> [WidgetNode s e]
mapEntities (PlayerIsExploring d _ _) = map (\e -> image (pack $ e ^. DT.imagePath) `styleBasic` [paddingL $ fromIntegral $ e ^. (position . _x) * tileWidth, paddingT $ fromIntegral $ mapDrawingHeight - ((e ^. (position . _y) + 1) * tileHeight)]) $ d ^. entities
mapEntities _                         = undefined

mapDrawingWidth, mapDrawingHeight :: Int
mapDrawingWidth = tileWidth * tileColumns
mapDrawingHeight = tileHeight * tileRows

tileWidth, tileHeight :: Int
tileWidth = 48
tileHeight = 48

tileColumns, tileRows :: Int
tileColumns = 17
tileRows = 9
