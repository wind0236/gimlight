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
import           Data.Maybe           (isNothing)
import           Dungeon              (Dungeon, actorAt, mapWidthAndHeight,
                                       pushActor, tileMap)
import           Dungeon.Map.Tile     (isWalkable)
import           Linear.V2            (V2 (V2))
import qualified Localization.Texts   as T

moveAction :: V2 Int -> Action
moveAction offset src d
    | not (movable d (src ^. position + offset)) = do
        tell [T.youCannotMoveThere]
        return $ ActionResult Failed (pushActor src d) []
    | otherwise =
        return $ ActionResult Ok (pushActor (updatePosition d src offset) d) []

updatePosition :: Dungeon -> Actor -> V2 Int -> Actor
updatePosition d src offset =
    let next = nextPosition d src offset
     in if movable d next
            then src & position .~ next
            else src

movable :: Dungeon -> Coord -> Bool
movable d c =
    isNothing (actorAt c d) &&
    isPositionInRange d c && isWalkable ((d ^. tileMap) ! c)

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
