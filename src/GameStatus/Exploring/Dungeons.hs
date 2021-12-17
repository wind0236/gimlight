module GameStatus.Exploring.Dungeons
    ( Dungeons
    , ascendStairsAtPlayerPosition
    , descendStairsAtPlayerPosition
    , exitDungeon
    , doPlayerAction
    , handleNpcTurns
    ) where

import           Action                     (Action,
                                             ActionResult (killed, newCellMap, status),
                                             ActionStatus)
import           Actor                      (Actor, isPlayer)
import qualified Actor.NpcBehavior          as NPC
import           Control.Lens               ((%%~), (&), (.~), (^.))
import           Control.Monad.Trans.Writer (Writer)
import           Data.Foldable              (find)
import           Data.Maybe                 (fromMaybe)
import           Dungeon                    (Dungeon, ascendingStairs, cellMap,
                                             descendingStairs,
                                             positionOnParentMap, updateMap)
import qualified Dungeon                    as D
import           Dungeon.Map.Cell           (locateActorAt, removeActorIf)
import           Dungeon.Map.Tile           (TileCollection)
import           Dungeon.Stairs             (StairsPair (StairsPair, downStairs, upStairs))
import           Log                        (MessageLog)
import           TreeZipper                 (TreeZipper, getFocused, goDownBy,
                                             goUp, modify)

type Dungeons = TreeZipper Dungeon

ascendStairsAtPlayerPosition :: TileCollection -> Dungeons -> Maybe Dungeons
ascendStairsAtPlayerPosition ts ds = newZipper
  where
    (player, zipperWithoutPlayer) = popPlayer ds
    ascendable =
        (downStairs <$> getFocused ds ^. ascendingStairs) ==
        D.playerPosition (getFocused ds)
    zipperFocusingNextDungeon = goUp zipperWithoutPlayer
    newPosition = upStairs <$> getFocused ds ^. ascendingStairs
    newZipper =
        case (zipperFocusingNextDungeon, newPosition, player, ascendable) of
            (Just g, Just pos, Just p, True) -> updateMapOrError g pos p
            _                                -> Nothing
    updateMapOrError g pos p =
        Just $
        modify
            (\d ->
                 fromMaybe
                     (error "Failed to update the map.")
                     (d & cellMap %%~ locateActorAt p pos >>= updateMap ts))
            g

descendStairsAtPlayerPosition :: TileCollection -> Dungeons -> Maybe Dungeons
descendStairsAtPlayerPosition ts ds = newZipper
  where
    (player, zipperWithoutPlayer) = popPlayer ds
    zipperFocusingNextDungeon =
        goDownBy
            (\x -> x ^. positionOnParentMap == currentPosition)
            zipperWithoutPlayer
    newPosition =
        downStairs <$>
        find
            (\(StairsPair from _) -> Just from == currentPosition)
            (getFocused ds ^. descendingStairs)
    currentPosition = D.playerPosition $ getFocused ds
    newZipper =
        case (zipperFocusingNextDungeon, newPosition, player) of
            (Just g, Just pos, Just p) -> updateMapOrError g pos p
            _                          -> Nothing
    updateMapOrError g pos p =
        Just $
        modify
            (\d ->
                 fromMaybe
                     (error "Failed to update the map.")
                     (d & cellMap %%~ locateActorAt p pos >>= updateMap ts))
            g

exitDungeon :: Dungeons -> Maybe Dungeons
exitDungeon ds = newZipper
  where
    (player, zipperWithoutPlayer) = popPlayer ds
    currentDungeon = getFocused ds
    newPosition = currentDungeon ^. positionOnParentMap
    zipperFocusingGlobalMap = goUp zipperWithoutPlayer
    newZipper =
        case (zipperFocusingGlobalMap, newPosition, player) of
            (Just g, Just pos, Just p) ->
                Just $
                modify
                    (\d ->
                         fromMaybe
                             (error "Failed to update the map.")
                             (d & cellMap %%~ locateActorAt p pos))
                    g
            _ -> Nothing

doPlayerAction ::
       Action
    -> TileCollection
    -> Dungeons
    -> Writer MessageLog (ActionStatus, Dungeons, [Actor])
doPlayerAction action ts ds = result
  where
    result = do
        actionResult <- action playerPos ts (getFocused ds ^. cellMap)
        let statusAndNewDungeon = (status actionResult, newCellMap actionResult)
        return $
            (\(a, cm) ->
                 (a, modify (\d -> d & cellMap .~ cm) ds, killed actionResult))
                statusAndNewDungeon
    playerPos =
        fromMaybe
            (error "Failed to get the player position")
            (D.playerPosition $ getFocused ds)

handleNpcTurns ::
       TileCollection -> Dungeons -> Writer MessageLog (Dungeons, [Actor])
handleNpcTurns ts ds =
    (\(x, ks) -> (modify (const x) ds, ks)) <$>
    NPC.handleNpcTurns ts (getFocused ds)

popPlayer :: Dungeons -> (Maybe Actor, Dungeons)
popPlayer z =
    case removeActorIf isPlayer (getFocused z ^. cellMap) of
        Just (actor, ncm) -> (Just actor, modify (\x -> x & cellMap .~ ncm) z)
        Nothing           -> (Nothing, z)
