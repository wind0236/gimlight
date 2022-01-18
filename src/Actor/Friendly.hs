module Actor.Friendly
    ( friendly
    ) where

import           Actor                   (Actor, ActorKind (FriendlyNpc), actor)
import           Actor.Identifier        (Identifier)
import           Actor.Status            (Status)
import           Control.Monad.State     (State)
import           Data.Text               (Text)
import           GameStatus.Talking.Part (TalkingPart)
import           IndexGenerator          (IndexGenerator)

friendly ::
       Identifier
    -> Status
    -> TalkingPart
    -> Text
    -> Text
    -> State IndexGenerator Actor
friendly name st p = actor name st FriendlyNpc (Just p)
