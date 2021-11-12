{-# LANGUAGE OverloadedStrings #-}

module Actor.Friendly.Electria
    ( electria
    ) where

import           Actor                   (Actor)
import           Actor.Friendly          (friendly)
import           Actor.Status            (status)
import           Actor.Status.Hp         (hp)
import           Coord                   (Coord)
import           Data.List.NonEmpty      (fromList)
import           GameStatus.Talking.Part (TalkingPart (Selection),
                                          selectionHandler)
import qualified Localization.Texts      as T

electria :: Coord -> Actor
electria position =
    friendly
        position
        T.electria
        st
        talking
        "images/electria.png"
        "images/sample_standing_picture.png"
  where
    st = status (hp 1) 1 1

talking :: TalkingPart
talking = q
  where
    q =
        Selection $selectionHandler T.talkWithElectria $
        fromList [(T.yes, Just afterYes), (T.no, Just afterNo)]
    afterYes =
        Selection $
        selectionHandler T.talkWithElectriaYes $ fromList [(T.yes, Nothing)]
    afterNo =
        Selection $selectionHandler T.talkWithElectriaNo $
        fromList [(T.yes, Nothing)]
