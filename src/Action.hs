module Action
    ( Action
    , ActionResult(..)
    , ActionResultWithLog
    , ActionStatus(..)
    ) where

import           Actor                      (Actor)
import           Control.Monad.Trans.Writer (Writer)
import           Coord                      (Coord)
import           Dungeon                    (Dungeon)
import           Dungeon.Map.Tile           (TileCollection)
import           Item.Book                  (Book)
import           Log                        (MessageLog)

data ActionStatus
    = Ok
    | ReadingStarted Book
    | Failed
    deriving (Show, Eq)

data ActionResult =
    ActionResult
        { status     :: ActionStatus
        , newDungeon :: Dungeon
        , killed     :: [Actor]
        }
    deriving (Show, Eq)

type Action = Coord -> Actor -> TileCollection -> Dungeon -> ActionResultWithLog

type ActionResultWithLog = Writer MessageLog ActionResult
