module UI.Draw.Map
    ( mapGrid
    ) where
import           Control.Lens     ((^.))
import           Coord            (Coord)
import           Data.Array       ((!))
import           Data.Text        (pack)
import           Dungeon          (mapWidthAndHeight, playerPosition)
import qualified Dungeon.Map.Tile as MT
import           Dungeon.Types    (Dungeon, entities, position, tileMap)
import qualified Dungeon.Types    as DT
import           Engine           (Engine (PlayerIsExploring))
import           Linear.V2        (V2 (V2), _x, _y)
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
    where V2 bottomLeftX bottomLeftY = bottomLeftCoord d
          rows = [hgrid $ row y | y <- [bottomLeftY + tileRows - 1, bottomLeftY + tileRows - 2 .. bottomLeftY]]
          row y = [cell (x, y) | x <- [bottomLeftX .. bottomLeftX + tileColumns - 1]]
          cell c = image $ pack $ (d ^. tileMap) ! c ^. MT.imagePath
          styles = [ width $ fromIntegral mapDrawingWidth
                   , height $ fromIntegral mapDrawingHeight]
mapTiles _ = undefined

mapEntities :: (WidgetModel s, WidgetEvent e) => Engine -> [WidgetNode s e]
mapEntities (PlayerIsExploring d _ _) = map entityToImage $ d ^. entities
    where leftPadding e = fromIntegral $ e ^. position . _x * tileWidth
          topPadding e = fromIntegral $ mapDrawingHeight - (e ^. (position . _y) + 1) * tileHeight

          style e = [paddingL $ leftPadding e, paddingT $ topPadding e]

          entityToImage e = image (pack $ e ^. DT.imagePath) `styleBasic` style e
mapEntities _                         = undefined

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
tileColumns = 17
tileRows = 9
