module Actor.Friendly
    ( friendly
    ) where

import           Actor                   (Actor, ActorKind (FriendlyNpc), actor)
import           Actor.Identifier        (Identifier)
import           Actor.Status            (Status)
import           Coord                   (Coord)
import           Data.Text               (Text)
import           GameStatus.Talking.Part (TalkingPart)

friendly ::
       Coord -> Identifier -> Status -> TalkingPart -> Text -> Text -> Actor
friendly position name st p = actor position name st FriendlyNpc (Just p)
