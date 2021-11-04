module GameModel.Status.Exploring.Dungeons
    ( Dungeons
    , ascendStairsAtPlayerPosition
    , descendStairsAtPlayerPosition
    , exitDungeon
    , doPlayerAction
    , handleNpcTurns
    ) where

import           Control.Lens               ((%~), (&), (.~), (^.))
import           Control.Monad.Trans.Writer (Writer)
import           Data.Foldable              (find)
import           Dungeon                    (Dungeon, actors, ascendingStairs,
                                             descendingStairs,
                                             positionOnParentMap, updateMap)
import qualified Dungeon                    as D
import           Dungeon.Actor              (Actor, isPlayer, position)
import           Dungeon.Actor.Actions      (Action, ActionStatus (Failed))
import qualified Dungeon.Actor.NpcBehavior  as NPC
import           Dungeon.Stairs             (StairsPair (StairsPair, downStairs, upStairs))
import           Log                        (MessageLog)
import           TreeZipper                 (TreeZipper, getFocused, goDownBy,
                                             goUp, modify)

type Dungeons = TreeZipper Dungeon

ascendStairsAtPlayerPosition :: Dungeons -> Maybe Dungeons
ascendStairsAtPlayerPosition ds = newZipper
    where (player, zipperWithoutPlayer) = popPlayer ds
          newPlayer = case (player, newPosition) of
                          (Just p, Just pos) -> Just $ p & position .~ pos
                          _                  -> Nothing
          ascendable = (downStairs <$> getFocused ds ^. ascendingStairs) == fmap (^. position) player
          zipperFocusingNextDungeon = goUp zipperWithoutPlayer
          newPosition = upStairs <$> getFocused ds ^. ascendingStairs
          newZipper = case (zipperFocusingNextDungeon, newPlayer, ascendable) of
                          (Just g, Just p, True) -> updateMapOrError g p
                          _                      -> Nothing
          updateMapOrError g p = Just $ modify (\d -> case updateMap $ d & actors %~ (:) p of
                                                          Just x -> x
                                                          Nothing -> error "Failed to update the map.") g

descendStairsAtPlayerPosition :: Dungeons -> Maybe Dungeons
descendStairsAtPlayerPosition ds = newZipper
    where (player, zipperWithoutPlayer) = popPlayer ds
          newPlayer = case (player, newPosition) of
                          (Just p, Just pos) -> Just $ p & position .~ pos
                          _                  -> Nothing
          zipperFocusingNextDungeon = goDownBy (\x -> x ^. positionOnParentMap == fmap (^. position) player) zipperWithoutPlayer
          newPosition = downStairs <$> find (\(StairsPair from _) -> Just from == fmap (^. position) player) (getFocused ds ^. descendingStairs)
          newZipper = case (zipperFocusingNextDungeon, newPlayer) of
                          (Just g, Just p) -> updateMapOrError g p
                          _                -> Nothing
          updateMapOrError g p = Just $ modify (\d -> case updateMap $ d & actors %~ (:) p of
                                                          Just x -> x
                                                          Nothing -> error "Failed to update the map.") g

exitDungeon :: Dungeons -> Maybe Dungeons
exitDungeon ds = newZipper
    where (player, zipperWithoutPlayer) = popPlayer ds
          currentDungeon = getFocused ds
          newPosition = currentDungeon ^. positionOnParentMap
          newPlayer = case (player, newPosition) of
                          (Just p, Just pos) -> Just $ p & position .~ pos
                          _                  -> Nothing
          zipperFocusingGlobalMap = goUp zipperWithoutPlayer
          newZipper = case (zipperFocusingGlobalMap, newPlayer) of
                          (Just g, Just p) -> Just $ modify (\d -> d & actors %~ (:) p) g
                          _                -> Nothing

doPlayerAction :: Action -> Dungeons -> Writer MessageLog (ActionStatus, Dungeons)
doPlayerAction action ds = result
    where (player, zipperWithoutPlayer) = popPlayer ds
          currentDungeonWithoutPlayer = getFocused zipperWithoutPlayer
          result = case player of
                       Just p  -> let dungeonAfterAction = action p currentDungeonWithoutPlayer
                                  in (\(a, d) -> (a, modify (const d) zipperWithoutPlayer)) <$> dungeonAfterAction
                       Nothing -> return (Failed, ds)

handleNpcTurns :: Dungeons -> Writer MessageLog Dungeons
handleNpcTurns ds = (\x -> modify (const x) ds) <$> NPC.handleNpcTurns (getFocused ds)

popPlayer :: Dungeons -> (Maybe Actor, Dungeons)
popPlayer = popActorIf isPlayer

popActorIf :: (Actor -> Bool) -> Dungeons -> (Maybe Actor, Dungeons)
popActorIf f z = (fst $ D.popActorIf f $ getFocused z, modify (snd . D.popActorIf f) z)
