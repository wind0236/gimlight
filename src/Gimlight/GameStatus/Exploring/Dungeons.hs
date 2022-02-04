module Gimlight.GameStatus.Exploring.Dungeons
  ( Dungeons,
    ascendStairsAtPlayerPosition,
    descendStairsAtPlayerPosition,
    exitDungeon,
    doPlayerAction,
    handleNpcTurns,
  )
where

import Control.Lens ((%%~), (&), (.~), (^.))
import Control.Monad.State (execStateT, runStateT)
import Control.Monad.Trans.Writer (Writer)
import Data.Either.Combinators (rightToMaybe)
import Data.Foldable (find)
import Gimlight.Action
  ( Action,
    ActionResult (killed, newCellMap, status),
    ActionStatus,
  )
import Gimlight.Actor (Actor, isPlayer)
import Gimlight.Data.Maybe (expectJust)
import Gimlight.Dungeon
  ( Dungeon,
    ascendingStairs,
    cellMap,
    descendingStairs,
    positionOnParentMap,
  )
import Gimlight.Dungeon.Map.Cell
  ( locateActorAt,
    playerActor,
    removeActorIf,
    updateExploredMap,
    updatePlayerFov,
  )
import Gimlight.Dungeon.Map.Tile (TileCollection)
import Gimlight.Dungeon.Stairs (StairsPair (StairsPair, downStairs, upStairs))
import Gimlight.Log (MessageLog)
import qualified Gimlight.NpcBehavior as NPC
import Gimlight.TreeZipper
  ( TreeZipper,
    getFocused,
    goDownBy,
    goUp,
    modify,
  )

type Dungeons = TreeZipper Dungeon

ascendStairsAtPlayerPosition :: TileCollection -> Dungeons -> Maybe Dungeons
ascendStairsAtPlayerPosition ts ds = newZipper
  where
    (player, zipperWithoutPlayer) = popPlayer ds
    ascendable =
      (downStairs <$> getFocused ds ^. ascendingStairs)
        == (fst <$> playerActor (getFocused ds ^. cellMap))
    zipperFocusingNextDungeon = goUp zipperWithoutPlayer
    newPosition = upStairs <$> getFocused ds ^. ascendingStairs
    newZipper =
      case (zipperFocusingNextDungeon, newPosition, player, ascendable) of
        (Just g, Just pos, Just p, True) -> updateMapOrError g pos p
        _ -> Nothing
    updateMapOrError g pos p =
      Just $
        modify
          ( \d ->
              expectJust
                "Failed to update the map."
                ( d & cellMap
                    %%~ ( \x ->
                            rightToMaybe (execStateT (locateActorAt ts p pos) x)
                              >>= updatePlayerFov ts
                              >>= Just
                                . updateExploredMap
                        )
                )
          )
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
      downStairs
        <$> find
          (\(StairsPair from _) -> Just from == currentPosition)
          (getFocused ds ^. descendingStairs)
    currentPosition = fmap fst . playerActor $ getFocused ds ^. cellMap
    newZipper =
      case (zipperFocusingNextDungeon, newPosition, player) of
        (Just g, Just pos, Just p) -> updateMapOrError g pos p
        _ -> Nothing
    updateMapOrError g pos p =
      Just $
        modify
          ( \d ->
              expectJust
                "Failed to update the map."
                ( d & cellMap
                    %%~ ( \x ->
                            rightToMaybe (execStateT (locateActorAt ts p pos) x)
                              >>= updatePlayerFov ts
                              >>= Just
                                . updateExploredMap
                        )
                )
          )
          g

exitDungeon :: TileCollection -> Dungeons -> Maybe Dungeons
exitDungeon ts ds = newZipper
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
              ( \d ->
                  expectJust
                    "Failed to update the map."
                    ( d & cellMap
                        %%~ rightToMaybe
                          . execStateT (locateActorAt ts p pos)
                    )
              )
              g
        _ -> Nothing

doPlayerAction ::
  Action ->
  TileCollection ->
  Dungeons ->
  Writer MessageLog (ActionStatus, Dungeons, [Actor])
doPlayerAction action ts ds = result
  where
    result = do
      actionResult <- action playerPos ts (getFocused ds ^. cellMap)
      let statusAndNewDungeon = (status actionResult, newCellMap actionResult)
      return $
        ( \(a, cm) ->
            (a, modify (\d -> d & cellMap .~ cm) ds, killed actionResult)
        )
          statusAndNewDungeon
    playerPos =
      maybe
        (error "Failed to get the player position")
        fst
        (playerActor $ getFocused ds ^. cellMap)

handleNpcTurns ::
  TileCollection -> Dungeons -> Writer MessageLog (Dungeons, [Actor])
handleNpcTurns ts ds =
  (\(x, ks) -> (modify (\d -> d & cellMap .~ x) ds, ks))
    <$> NPC.handleNpcTurns ts (getFocused ds ^. cellMap)

popPlayer :: Dungeons -> (Maybe Actor, Dungeons)
popPlayer z =
  case flip runStateT (getFocused z ^. cellMap) $ removeActorIf isPlayer of
    Right (actor, ncm) -> (Just actor, modify (\x -> x & cellMap .~ ncm) z)
    _ -> (Nothing, z)
