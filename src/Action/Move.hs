module Action.Move
    ( moveAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionStatus (Failed, Ok))
import           Actor                (Actor, position)
import           Control.Lens         ((&), (.~), (^.))
import           Control.Monad.Writer (tell)
import           Coord                (Coord)
import           Data.Array           ((!))
import           Data.Foldable        (find)
import           Data.Maybe           (isJust)
import           Dungeon              (Dungeon, getActors, mapWidthAndHeight,
                                       pushActor, tileMap)
import           Dungeon.Map.Tile     (TileCollection, isWalkable)
import           Linear.V2            (V2 (V2))
import qualified Localization.Texts   as T

moveAction :: V2 Int -> Action
moveAction offset src tiles d
    | not (movable tiles d (src ^. position + offset)) = do
        tell [T.youCannotMoveThere]
        return $ ActionResult Failed (pushActor src d) []
    | otherwise =
        return $
        ActionResult Ok (pushActor (updatePosition tiles d src offset) d) []

updatePosition :: TileCollection -> Dungeon -> Actor -> V2 Int -> Actor
updatePosition ts d src offset =
    let next = nextPosition d src offset
     in if movable ts d next
            then src & position .~ next
            else src

movable :: TileCollection -> Dungeon -> Coord -> Bool
movable ts d c =
    not actorExistsAtDestination &&
    isPositionInRange d c && isWalkable (ts ! ((d ^. tileMap) ! c))
  where
    actorExistsAtDestination =
        isJust $ find (\x -> x ^. position == c) $ getActors d

nextPosition :: Dungeon -> Actor -> V2 Int -> Coord
nextPosition d src offset =
    max (V2 0 0) $ min (V2 (width - 1) $ height - 1) $ src ^. position + offset
  where
    V2 width height = mapWidthAndHeight d

isPositionInRange :: Dungeon -> Coord -> Bool
isPositionInRange d c = x >= 0 && x < width && y >= 0 && y < height
  where
    V2 width height = mapWidthAndHeight d
    V2 x y = c
