module Actor.Friendly
    ( friendly
    ) where

import           Actor        (Actor, ActorKind (FriendlyNpc), actor)
import           Actor.Status (Status)
import           Coord        (Coord)
import           Data.Text    (Text)
import           Localization (MultilingualText)

friendly ::
       Coord
    -> MultilingualText
    -> Status
    -> MultilingualText
    -> Text
    -> Text
    -> Actor
friendly position name st = actor position name st FriendlyNpc
