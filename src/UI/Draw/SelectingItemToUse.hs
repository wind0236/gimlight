{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.SelectingItemToUse
    ( drawSelectingItemToUse
    ) where

import           GameConfig                    (GameConfig)
import           GameStatus.SelectingItemToUse (SelectingItemToUseHandler,
                                                getItems, getSelectingIndex)
import           Item                          (getName)
import           Localization                  (getLocalizedText)
import qualified Localization.Texts            as T
import           Monomer                       (label, vstack)
import           TextShow                      (TextShow (showt))
import           UI.Draw.KeyEvent              (withKeyEvents)
import           UI.Types                      (GameWidgetNode)

drawSelectingItemToUse ::
       SelectingItemToUseHandler -> GameConfig -> GameWidgetNode
drawSelectingItemToUse sh c = withKeyEvents $ vstack labels
  where
    labels = label topLabel : map label addAsterlist
    addAsterlist =
        zipWith
            (\idx x ->
                 if Just idx == getSelectingIndex sh
                     then "* " <> showt idx <> " " <> x
                     else showt idx <> " " <> x)
            [0 ..] $
        map (getLocalizedText c) itemNames
    itemNames = map getName $ getItems sh
    topLabel = getLocalizedText c T.whichItemToUse
