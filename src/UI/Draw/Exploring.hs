{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Exploring
    ( drawExploring
    ) where

import           Actor                           (getCurrentExperiencePoint,
                                                  getDefence,
                                                  getExperiencePointForNextLevel,
                                                  getHp, getLevel, getMaxHp,
                                                  getPower, walkingImagePath)
import           Codec.Picture                   (Image (imageData),
                                                  PixelRGBA8 (PixelRGBA8),
                                                  pixelMap)
import           Control.Applicative             (ZipList (ZipList, getZipList))
import           Control.Lens                    ((^.))
import           Control.Monad                   (guard)
import           Coord                           (Coord)
import           Data.Array                      ((!))
import           Data.Default                    (Default (def))
import           Data.Maybe                      (fromMaybe, mapMaybe)
import           Data.Vector.Split               (chunksOf)
import qualified Data.Vector.Storable            as V
import           Data.Vector.Storable.ByteString (vectorToByteString)
import           Dungeon                         (Dungeon, cellMap,
                                                  getPositionsAndActors,
                                                  mapWidthAndHeight,
                                                  playerPosition, visible)
import           Dungeon.Map.Cell                (exploredMap,
                                                  positionsAndItems, tileIdAt)
import           GameConfig                      (GameConfig)
import           GameStatus.Exploring            (ExploringHandler,
                                                  getCurrentDungeon,
                                                  getMessageLog, getPlayerActor)
import qualified Item                            as I
import           Linear.V2                       (V2 (V2), _x, _y)
import           Localization                    (getLocalizedText)
import qualified Localization.Texts              as T
import           Monomer                         (CmbHeight (height),
                                                  CmbMultiline (multiline),
                                                  CmbPaddingL (paddingL),
                                                  CmbPaddingT (paddingT),
                                                  CmbStyleBasic (styleBasic),
                                                  CmbWidth (width),
                                                  Point (Point), Rect (Rect),
                                                  Renderer (addImage, beginPath, deleteImage, fill, setFillImagePattern),
                                                  Size (Size), Widget,
                                                  WidgetNode, currentStyle,
                                                  defaultWidgetNode,
                                                  drawRoundedRect,
                                                  getContentArea, hstack, image,
                                                  label, label_, vstack, zstack)
import           Monomer.Widgets.Single          (Single (singleRender),
                                                  createSingle)
import           TextShow                        (TextShow (showt))
import           UI.Draw.Config                  (logRows, tileColumns,
                                                  tileHeight, tileRows,
                                                  tileWidth, windowWidth)
import           UI.Draw.KeyEvent                (withKeyEvents)
import           UI.Graphics.MapTiles            (MapTiles)
import           UI.Types                        (GameWidgetNode)

drawExploring :: MapTiles -> ExploringHandler -> GameConfig -> GameWidgetNode
drawExploring tileGraphics eh c =
    withKeyEvents $ vstack [statusAndMapGrid, messageLogArea eh c]
  where
    statusAndMapGrid =
        hstack
            [ mapGrid tileGraphics eh
            , statusGrid eh c `styleBasic`
              [width $ fromIntegral $ windowWidth - tileWidth * tileColumns]
            ]

messageLogArea :: ExploringHandler -> GameConfig -> GameWidgetNode
messageLogArea eh c =
    vstack $
    fmap (\x -> label_ (getLocalizedText c x) [multiline]) $
    take logRows $ getMessageLog eh

mapGrid :: MapTiles -> ExploringHandler -> GameWidgetNode
mapGrid tileGraphics eh =
    zstack (mapWidget tileGraphics eh : (mapItems eh ++ mapActors eh)) `styleBasic`
    [ width $ fromIntegral mapDrawingWidth
    , height $ fromIntegral mapDrawingHeight
    ]

statusGrid :: ExploringHandler -> GameConfig -> GameWidgetNode
statusGrid eh c =
    vstack $
    maybe
        []
        (\x ->
             [ label "Player"
             , label $ lvl <> ": " <> showt (getLevel x)
             , label $
               experience <>
               ": " <>
               showt (getCurrentExperiencePoint x) <>
               " / " <> showt (getExperiencePointForNextLevel x)
             , label $ "HP: " <> showt (getHp x) <> " / " <> showt (getMaxHp x)
             , label $ atk <> ": " <> showt (getPower x)
             , label $ defence <> ": " <> showt (getDefence x)
             ]) $
    getPlayerActor eh
  where
    lvl = getLocalizedText c T.level
    experience = getLocalizedText c T.experience
    atk = getLocalizedText c T.attack
    defence = getLocalizedText c T.defence

mapWidget :: MapTiles -> ExploringHandler -> WidgetNode s e
mapWidget tiles eh = defaultWidgetNode "map" $ makeMap tiles eh

makeMap :: MapTiles -> ExploringHandler -> Widget s e
makeMap tileGraphics eh = createSingle () def {singleRender = render}
  where
    render wenv node renderer = do
        addImage renderer imagePath mapSize rows []
        beginPath renderer
        setFillImagePattern
            renderer
            imagePath
            (Point x y)
            (Size w h)
            angle
            transparent
        drawRoundedRect renderer (Rect x y w h) def
        fill renderer
        deleteImage renderer imagePath
      where
        style = currentStyle wenv node
        Rect x y w h = getContentArea node style
        angle = 0
        transparent = 1
    rows =
        vectorToByteString $
        V.concat [row y | y <- [topLeftCoordY .. topLeftCoordY + tileRows - 1]]
    row y =
        V.concat $
        getZipList $
        foldl1
            (\acc x -> (V.++) <$> acc <*> x)
            [ ZipList $ imageAt $ V2 x y
            | x <- [topLeftCoordX .. topLeftCoordX + tileColumns - 1]
            ]
    imageAt c =
        chunksOf (tileWidth * 4) $ -- `(*4)` for R, G, B, and A bytes.
        imageData $ pixelMap (applyOpacity c) $ tileGraphics ! tileId c
    tileId c =
        fromMaybe
            (error "Failed to get the tile ID.")
            (tileIdAt c (d ^. cellMap))
    applyOpacity c (PixelRGBA8 r g b a)
        | isVisible c = PixelRGBA8 r g b a
        | isExplored c = PixelRGBA8 (r `div` 2) (g `div` 2) (b `div` 2) a
        | otherwise = PixelRGBA8 0 0 0 0xff
    isVisible c = (d ^. visible) ! c
    isExplored c = exploredMap (d ^. cellMap) ! c
    d = getCurrentDungeon eh
    V2 topLeftCoordX topLeftCoordY = topLeftCoord d
    imagePath = "mapWidget"

mapItems :: ExploringHandler -> [GameWidgetNode]
mapItems eh = mapMaybe itemToImage $ positionsAndItems $ d ^. cellMap
  where
    itemToImage (position, item) =
        guard (isItemDrawed position) >>
        return (image (I.getIconImagePath item) `styleBasic` style position)
    isItemDrawed position =
        let displayPosition = itemPositionOnDisplay position
            isVisible = (d ^. visible) ! position
         in V2 0 0 <= displayPosition &&
            displayPosition < V2 tileColumns tileRows && isVisible
    d = getCurrentDungeon eh
    leftPadding position =
        fromIntegral $ itemPositionOnDisplay position ^. _x * tileWidth
    topPadding position =
        fromIntegral $ itemPositionOnDisplay position ^. _y * tileHeight
    style position =
        [paddingL $ leftPadding position, paddingT $ topPadding position]
    itemPositionOnDisplay position = position - topLeftCoord d

mapActors :: ExploringHandler -> [GameWidgetNode]
mapActors eh = mapMaybe actorToImage $ getPositionsAndActors d
  where
    d = getCurrentDungeon eh
    leftPadding actor =
        fromIntegral $ actorPositionOnDisplay actor ^. _x * tileWidth
    topPadding actor =
        fromIntegral $ (actorPositionOnDisplay actor ^. _y) * tileHeight
    style position =
        [paddingL $ leftPadding position, paddingT $ topPadding position]
    actorPositionOnDisplay position = position - topLeftCoord d
    isActorDrawed position =
        let displayPosition = actorPositionOnDisplay position
            isVisible = (d ^. visible) ! position
         in V2 0 0 <= displayPosition &&
            displayPosition < V2 tileColumns tileRows && isVisible
    actorToImage (position, actor) =
        guard (isActorDrawed position) >>
        return (image (actor ^. walkingImagePath) `styleBasic` style position)

topLeftCoord :: Dungeon -> Coord
topLeftCoord d = V2 x y
  where
    V2 unadjustedX unadjestedY =
        maybe
            (V2 0 0)
            (\pos -> pos - V2 (tileColumns `div` 2) (tileRows `div` 2))
            (playerPosition d)
    V2 maxX maxY = mapWidthAndHeight d - V2 tileColumns tileRows
    x = max 0 $ min maxX unadjustedX
    y = max 0 $ min maxY unadjestedY

mapSize :: Size
mapSize = Size (fromIntegral mapDrawingWidth) (fromIntegral mapDrawingHeight)

mapDrawingWidth :: Int
mapDrawingWidth = tileWidth * tileColumns

mapDrawingHeight :: Int
mapDrawingHeight = tileHeight * tileRows
