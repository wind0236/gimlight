module Dungeon.Actor.Actions
    ( Action
    , ActionResult
    ) where

import           Control.Monad.Trans.Maybe  (MaybeT)
import           Control.Monad.Trans.Writer (Writer)
import           Dungeon                    (Dungeon)
import           Dungeon.Actor              (Actor)
import           Log                        (MessageLog)

type Action = Actor -> Dungeon -> ActionResult

type ActionResult = MaybeT (Writer MessageLog) Dungeon
