module Actor.Friendly
    ( friendly
    ) where

import           Actor                   (Actor, ActorKind (FriendlyNpc), actor)
import           Actor.Identifier        (Identifier)
import           Actor.Status            (Status)
import           Data.Text               (Text)
import           GameStatus.Talking.Part (TalkingPart)
import           IndexGenerator          (IndexGenerator)

friendly ::
       IndexGenerator
    -> Identifier
    -> Status
    -> TalkingPart
    -> Text
    -> Text
    -> (Actor, IndexGenerator)
friendly ig name st p = actor ig name st FriendlyNpc (Just p)
