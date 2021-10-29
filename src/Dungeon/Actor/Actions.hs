module Dungeon.Actor.Actions
    ( Action
    , ActionResult
    ) where

import           Dungeon       (Dungeon)
import           Dungeon.Actor (Actor)
import           Log           (MessageWriter)

type Action = Actor -> Dungeon -> ActionResult

type ActionResult = MessageWriter (Bool, Dungeon)
