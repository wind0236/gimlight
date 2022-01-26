module Gimlight.Action
    ( Action
    , ActionResult(..)
    , ActionResultWithLog
    , ActionStatus(..)
    ) where

import           Control.Monad.Trans.Writer (Writer)
import           Gimlight.Actor             (Actor)
import           Gimlight.Coord             (Coord)
import           Gimlight.Dungeon.Map.Cell  (CellMap)
import           Gimlight.Dungeon.Map.Tile  (TileCollection)
import           Gimlight.Item.Book         (Book)
import           Gimlight.Log               (MessageLog)

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
