module Action
    ( Action
    , ActionResult(..)
    , ActionResultWithLog
    , ActionStatus(..)
    ) where

import           Actor                      (Actor)
import           Control.Monad.Trans.Writer (Writer)
import           Dungeon                    (Dungeon)
import           Item.Book                  (Book)
import           Log                        (MessageLog)

data ActionStatus
    = Ok
    | ReadingStarted Book
    | Failed

data ActionResult =
    ActionResult
        { status     :: ActionStatus
        , newDungeon :: Dungeon
        }

type Action = Actor -> Dungeon -> ActionResultWithLog

type ActionResultWithLog = Writer MessageLog ActionResult
