{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Exploring
    ( drawExploring
    ) where

import           Control.Lens                   ((&), (.~), (^.))
import           Control.Monad                  (guard)
import           Coord                          (Coord)
import           Data.Array                     ((!))
import           Data.Maybe                     (mapMaybe)
import           Dungeon                        (Dungeon, actors, explored,
                                                 items, mapWidthAndHeight,
                                                 playerPosition, tileMap,
                                                 visible)
import           Dungeon.Actor                  (getDefence, getHp, getMaxHp,
                                                 getPower, walkingImagePath)
import qualified Dungeon.Actor                  as A
import           Dungeon.Item                   (iconImagePath)
import qualified Dungeon.Item                   as I
import qualified Dungeon.Map.Tile               as MT
import           Game                           (Game (Game, config, status))
import           Game.Status                    (GameStatus (Exploring, HandlingScene, SelectingItemToUse, Talking))
import           Game.Status.Exploring          (getCurrentDungeon,
                                                 getMessageLog, getPlayerActor)
import qualified Game.Status.Scene              as S
import           Game.Status.SelectingItemToUse (finishSelecting)
import qualified Game.Status.Talking            as T
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
                                                 CmbWidth (width), black, box_,
                                                 filler, hgrid, hstack, image,
                                                 label, label_, vgrid, vstack,
                                                 zstack)
import qualified Monomer.Lens                   as L
import           TextShow                       (TextShow (showt))
import           UI.Draw.Config                 (logRows, tileColumns,
                                                 tileHeight, tileRows,
                                                 tileWidth, windowWidth)
import           UI.Draw.KeyEvent               (withKeyEvents)
import           UI.Types                       (GameWidgetNode)

drawExploring :: Game -> GameWidgetNode
drawExploring gs = withKeyEvents $ vstack [ statusAndMapGrid
                                        , messageLogArea gs
                                        ]
    where statusAndMapGrid = hstack [ mapGrid gs
                                    , statusGrid gs `styleBasic` [width $ fromIntegral $ windowWidth - tileWidth * tileColumns]
                                    ]

messageLogArea :: Game -> GameWidgetNode
messageLogArea Game { status = s, config = c } =
    vstack $ fmap (\x -> label_ (getLocalizedText c x) [multiline] ) $ take logRows $ ls s
    where ls st = case st of
                   Exploring eh     -> getMessageLog eh
                   Talking th       -> getMessageLog $ snd $ T.destructHandler th
                   HandlingScene sh -> getMessageLog $ snd $ S.destructHandler sh
                   _                -> error "unable to print logs."

mapGrid :: Game -> GameWidgetNode
mapGrid gs = zstack (mapTiles gs:(mapItems gs ++ mapActors gs))
    `styleBasic` [ width $ fromIntegral mapDrawingWidth
                 , height $ fromIntegral mapDrawingHeight
                 ]

statusGrid :: Game -> GameWidgetNode
statusGrid Game { status = s, config = c } = vstack $ maybe []
    (\x -> [ label "Player"
           , label $ "HP: " <> showt (getHp x) <> " / " <> showt (getMaxHp x)
           , label $ atk <> showt (getPower x)
           , label $ def <> showt (getDefence x)
           ]) $ player s
    where atk = getLocalizedText c $ multilingualText "ATK: " "攻撃: "
          def = getLocalizedText c $ multilingualText "DEF: " "防御: "
          player st = case st of
                       Exploring eh -> getPlayerActor eh
                       Talking th   -> getPlayerActor $ snd $ T.destructHandler th
                       HandlingScene sh -> getPlayerActor $ snd $ S.destructHandler sh
                       SelectingItemToUse i -> player $ Exploring $ finishSelecting i
                       _ -> error "No player entity."

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
                      Talking th       -> snd $ T.destructHandler th
                      HandlingScene sh -> snd $ S.destructHandler sh
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
                      Talking th       -> snd $ T.destructHandler th
                      HandlingScene sh -> snd $ S.destructHandler sh
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
                      Talking th       -> snd $ T.destructHandler th
                      HandlingScene sh -> snd $ S.destructHandler sh
                      _                -> error "unreachable."

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
