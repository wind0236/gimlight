module Action
    ( Action
    , ActionResult(..)
    , ActionResultWithLog
    , ActionStatus(..)
    ) where

import           Actor                      (Actor)
import           Control.Monad.Trans.Writer (Writer)
import           Coord                      (Coord)
import           Dungeon.Map.Cell           (CellMap)
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
        , newCellMap :: CellMap
        , killed     :: [Actor]
        }
    deriving (Show, Eq)

type Action = Coord -> TileCollection -> CellMap -> ActionResultWithLog

type ActionResultWithLog = Writer MessageLog ActionResult
