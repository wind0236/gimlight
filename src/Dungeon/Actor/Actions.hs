module Dungeon.Actor.Actions
    ( Action
    ) where

import           Dungeon       (Dungeon)
import           Dungeon.Actor (Actor)
import           Log           (MessageLog)

type Action = Actor -> Dungeon -> ((MessageLog, Bool), Dungeon)
