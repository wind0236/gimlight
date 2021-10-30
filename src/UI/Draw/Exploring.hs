{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Exploring
    ( drawExploring
    ) where

import           Control.Lens               ((&), (.~), (^.))
import           Control.Monad              (guard)
import           Coord                      (Coord)
import           Data.Array                 ((!))
import           Data.Maybe                 (mapMaybe)
import           Dungeon                    (Dungeon, actors, explored, items,
                                             mapWidthAndHeight, playerPosition,
                                             tileMap, visible)
import           Dungeon.Actor              (getDefence, getHp, getMaxHp,
                                             getPower, walkingImagePath)
import qualified Dungeon.Actor              as A
import           Dungeon.Item               (iconImagePath)
import qualified Dungeon.Item               as I
import qualified Dungeon.Map.Tile           as MT
import           GameModel.Config           (Config)
import           GameModel.Status.Exploring (ExploringHandler,
                                             getCurrentDungeon, getMessageLog,
                                             getPlayerActor)
import           Linear.V2                  (V2 (V2), _x, _y)
import           Localization               (getLocalizedText)
import qualified Localization.Texts         as T
import           Monomer                    (CmbAlignLeft (alignLeft),
                                             CmbBgColor (bgColor),
                                             CmbHeight (height),
                                             CmbMultiline (multiline),
                                             CmbPaddingL (paddingL),
                                             CmbPaddingT (paddingT),
                                             CmbStyleBasic (styleBasic),
                                             CmbWidth (width), black, box_,
                                             filler, hgrid, hstack, image,
                                             label, label_, vgrid, vstack,
                                             zstack)
import qualified Monomer.Lens               as L
import           TextShow                   (TextShow (showt))
import           UI.Draw.Config             (logRows, tileColumns, tileHeight,
                                             tileRows, tileWidth, windowWidth)
import           UI.Draw.KeyEvent           (withKeyEvents)
import           UI.Types                   (GameWidgetNode)

drawExploring :: ExploringHandler -> Config -> GameWidgetNode
drawExploring eh c = withKeyEvents $ vstack [ statusAndMapGrid
                                            , messageLogArea eh c
                                            ]
    where statusAndMapGrid = hstack [ mapGrid eh
                                    , statusGrid eh c `styleBasic` [width $ fromIntegral $ windowWidth - tileWidth * tileColumns]
                                    ]

messageLogArea :: ExploringHandler -> Config -> GameWidgetNode
messageLogArea eh c =
    vstack $ fmap (\x -> label_ (getLocalizedText c x) [multiline] ) $ take logRows $ getMessageLog eh

mapGrid :: ExploringHandler -> GameWidgetNode
mapGrid eh = zstack (mapTiles eh:(mapItems eh ++ mapActors eh))
    `styleBasic` [ width $ fromIntegral mapDrawingWidth
                 , height $ fromIntegral mapDrawingHeight
                 ]

statusGrid :: ExploringHandler -> Config -> GameWidgetNode
statusGrid eh c = vstack $ maybe []
    (\x -> [ label "Player"
           , label $ "HP: " <> showt (getHp x) <> " / " <> showt (getMaxHp x)
           , label $ atk <> ": " <> showt (getPower x)
           , label $ def <> ": " <> showt (getDefence x)
           ]) $ getPlayerActor eh
    where atk = getLocalizedText c T.attack
          def = getLocalizedText c T.defence

mapTiles :: ExploringHandler -> GameWidgetNode
mapTiles eh = box_ [alignLeft] $ vgrid rows `styleBasic` styles
    where d = getCurrentDungeon eh
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

mapItems :: ExploringHandler -> [GameWidgetNode]
mapItems eh = mapMaybe itemToImage $ d ^. items
    where itemToImage item = guard (isItemDrawed item) >> return (image (item ^. iconImagePath) `styleBasic` style item)
          isItemDrawed item = let pos = itemPositionOnDisplay item
                                  isVisible = (d ^. visible) ! (item ^. I.position)
                                in V2 0 0 <= pos && pos <= topRightCoord d && isVisible
          d = getCurrentDungeon eh
          leftPadding item = fromIntegral $ itemPositionOnDisplay item ^. _x * tileWidth
          topPadding item = fromIntegral $ mapDrawingHeight - (itemPositionOnDisplay item ^. _y + 1) * tileHeight

          style item = [paddingL $ leftPadding item, paddingT $ topPadding item]

          itemPositionOnDisplay item = item ^. I.position - bottomLeftCoord d

mapActors :: ExploringHandler -> [GameWidgetNode]
mapActors eh = mapMaybe actorToImage $ d ^. actors
    where d = getCurrentDungeon eh
          leftPadding actor = fromIntegral $ actorPositionOnDisplay actor ^. _x * tileWidth
          topPadding actor = fromIntegral $ mapDrawingHeight - (actorPositionOnDisplay actor ^. _y + 1) * tileHeight

          style actor = [paddingL $ leftPadding actor, paddingT $ topPadding actor]

          actorPositionOnDisplay actor = actor ^. A.position - bottomLeftCoord d

          isActorDrawed actor = let pos = actorPositionOnDisplay actor
                                    isVisible = (d ^. visible) ! (actor ^. A.position)
                                in V2 0 0 <= pos && pos <= topRightCoord d && isVisible

          actorToImage actor = guard (isActorDrawed actor) >> return (image (actor ^. walkingImagePath) `styleBasic` style actor)

bottomLeftCoord :: Dungeon -> Coord
bottomLeftCoord d = V2 x y
    where V2 unadjustedX unadjestedY = maybe (V2 0 0) (\pos -> pos - V2 (tileColumns `div` 2) (tileRows `div` 2)) (playerPosition d)
          V2 maxX maxY = mapWidthAndHeight d - V2 tileColumns tileRows
          x = max 0 $ min maxX unadjustedX
          y = max 0 $ min maxY unadjestedY

topRightCoord :: Dungeon -> Coord
topRightCoord d = bottomLeftCoord d + mapWidthAndHeight d - V2 1 1

mapDrawingWidth, mapDrawingHeight :: Int
mapDrawingWidth = tileWidth * tileColumns
mapDrawingHeight = tileHeight * tileRows
