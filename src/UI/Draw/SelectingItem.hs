{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.SelectingItem
    ( drawSelectingItem
    ) where

import           GameConfig               (GameConfig)
import           GameStatus.SelectingItem (Reason (Drop, Use),
                                           SelectingItemHandler, getItems,
                                           getReason, getSelectingIndex)
import           Item                     (getName)
import           Localization             (getLocalizedText)
import qualified Localization.Texts       as T
import           Monomer                  (label, vstack)
import           TextShow                 (TextShow (showt))
import           UI.Draw.KeyEvent         (withKeyEvents)
import           UI.Types                 (GameWidgetNode)

drawSelectingItem :: SelectingItemHandler -> GameConfig -> GameWidgetNode
drawSelectingItem sh c = withKeyEvents $ vstack labels
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
    topLabel = getLocalizedText c topLabelText
    topLabelText =
        case getReason sh of
            Use  -> T.whatToUse
            Drop -> T.whatToDrop
