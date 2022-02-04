{-# LANGUAGE OverloadedStrings #-}

module Gimlight.UI.Draw.SelectingItem
  ( drawSelectingItem,
  )
where

import Gimlight.GameConfig (GameConfig)
import Gimlight.GameStatus.SelectingItem
  ( Reason (Drop, Use),
    SelectingItemHandler,
    getItems,
    getReason,
    getSelectingIndex,
  )
import Gimlight.Item (getName)
import Gimlight.Localization (getLocalizedText)
import qualified Gimlight.Localization.Texts as T
import Gimlight.UI.Draw.KeyEvent (withKeyEvents)
import Gimlight.UI.Types (GameWidgetNode)
import Monomer (label, vstack)
import TextShow (TextShow (showt))

drawSelectingItem :: SelectingItemHandler -> GameConfig -> GameWidgetNode
drawSelectingItem sh c = withKeyEvents $ vstack labels
  where
    labels = label topLabel : map label addAsterlist
    addAsterlist =
      zipWith
        ( \idx x ->
            if Just idx == getSelectingIndex sh
              then "* " <> showt idx <> " " <> x
              else showt idx <> " " <> x
        )
        [0 ..]
        $ map (getLocalizedText c) itemNames
    itemNames = map getName $ getItems sh
    topLabel = getLocalizedText c topLabelText
    topLabelText =
      case getReason sh of
        Use -> T.whatToUse
        Drop -> T.whatToDrop
