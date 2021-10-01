module UI.Attrs
    ( attrMapForThisGame
    , emptyAttr
    , hpBarFilled
    , hpBarEmpty
    ) where

import           Brick.AttrMap                 (AttrMap, AttrName, attrMap,
                                                attrName)
import           Brick.Util                    (on)
import           Graphics.Vty.Attributes       (defAttr)
import           Graphics.Vty.Attributes.Color (black, rgbColor)

attrMapForThisGame :: AttrMap
attrMapForThisGame = attrMap defAttr
    [ (playerAttr           , rgbColor 255 255 255 `on` black)
    , (orcAttr              , rgbColor  63 127  63 `on` black)
    , (trollAttr            , rgbColor   0 127   0 `on` black)
    , (deadAttr             , rgbColor 191   0   0 `on` black)
    , (darkFloorAttr        , rgbColor 128 128 128 `on` black)
    , (lightFloorAttr       , rgbColor 255 255 255 `on` black)
    , (darkWallAttr         , rgbColor 128 128 128 `on` black)
    , (lightWallAttr        , rgbColor 255 255 255 `on` black)
    , (hpBarFilled          , rgbColor   0 255   0 `on` black)
    , (hpBarEmpty           , rgbColor 255   0   0 `on` black)
    , (attackMessageAttr    , rgbColor 255   0   0 `on` black)
    , (infoMessageAttr      , rgbColor   0 255   0 `on` black)
    , (emptyAttr            , black                `on` black)
    ]

playerAttr, npcAttr, emptyAttr, darkFloorAttr, darkWallAttr, orcAttr, trollAttr, infoMessageAttr, hpBarFilled, hpBarEmpty, deadAttr :: AttrName
playerAttr        = attrName "playerAttr"
npcAttr           = attrName "npcAttr"
emptyAttr         = attrName "emptyAttr"
darkFloorAttr     = attrName "darkFloorAttr"
lightFloorAttr    = attrName "lightFloorAttr"
darkWallAttr      = attrName "darkWallAttr"
lightWallAttr     = attrName "lightWallAttr"
orcAttr           = attrName "orcAttr"
trollAttr         = attrName "trollAttr"
attackMessageAttr = attrName "attackMessageAttr"
infoMessageAttr   = attrName "infoMessageAttr"
hpBarFilled       = attrName "hpBarFilled"
hpBarEmpty        = attrName "hpBarEmpty"
deadAttr          = attrName "deadAttr"
