module Actor.Friendly
    ( friendly
    ) where

import           Actor                   (Actor, ActorKind (FriendlyNpc), actor)
import           Actor.Identifier        (Identifier)
import           Actor.Status            (Status)
import           Coord                   (Coord)
import           Data.Text               (Text)
import           GameStatus.Talking.Part (TalkingPart)
import           IndexGenerator          (IndexGenerator)

friendly ::
       IndexGenerator
    -> Coord
    -> Identifier
    -> Status
    -> TalkingPart
    -> Text
    -> Text
    -> (Actor, IndexGenerator)
friendly ig position name st p = actor ig position name st FriendlyNpc (Just p)
