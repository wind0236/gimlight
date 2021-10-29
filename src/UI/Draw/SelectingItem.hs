{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.SelectingItem
    ( drawSelectingItem
    ) where

import           Control.Lens                   ((^.))
import           Dungeon.Item                   (name)
import           Game                           (Game (Game, config, status))
import           Game.Status                    (GameStatus (SelectingItemToUse))
import           Game.Status.SelectingItemToUse (getItems, getSelectingIndex)
import           Localization                   (getLocalizedText,
                                                 multilingualText)
import           Monomer                        (label, vstack)
import           TextShow                       (TextShow (showt))
import           UI.Draw.KeyEvent               (withKeyEvents)
import           UI.Types                       (GameWidgetNode)

drawSelectingItem :: Game -> GameWidgetNode
drawSelectingItem Game { status = SelectingItemToUse sh, config = c } = withKeyEvents $ vstack labels
    where labels = label topLabel:map label addAsterlist
          addAsterlist = zipWith (\idx x -> if Just idx == getSelectingIndex sh
                                                then "* " <> showt idx <> " " <> x
                                                else showt idx <> " " <> x
                                               ) [0..] $ map (getLocalizedText c) itemNames
          itemNames = map (^. name) $ getItems sh
          topLabel = getLocalizedText c $ multilingualText "Which Item do you use?" "どのアイテムを使う？"
drawSelectingItem _ = error "We are not selecting an item."
