module UI.Attrs
    ( attrMapForThisGame
    , whiteAttr
    , emptyAttr
    , redAttr
    , grayAttr
    , greenAttr
    ) where

import           Brick.AttrMap                 (AttrMap, AttrName, attrMap,
                                                attrName)
import           Brick.Util                    (on)
import           Graphics.Vty.Attributes       (defAttr)
import           Graphics.Vty.Attributes.Color (black, rgbColor)

attrMapForThisGame :: AttrMap
attrMapForThisGame = attrMap defAttr
    [ (whiteAttr            , rgbColor 255 255 255 `on` black)
    , (grayAttr             , rgbColor  63  63  63 `on` black)
    , (redAttr              , rgbColor 255   0   0 `on` black)
    , (greenAttr            , rgbColor   0 255   0 `on` black)
    , (emptyAttr            , black                `on` black)
    ]

whiteAttr, grayAttr, redAttr, greenAttr, emptyAttr :: AttrName
whiteAttr         = attrName "whiteAttr"
grayAttr          = attrName "grayAttr"
redAttr           = attrName "redAttr"
greenAttr         = attrName "greenAttr"
emptyAttr         = attrName "emptyAttr"
