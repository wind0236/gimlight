module GameModel.Status.Exploring.Dungeons
    ( Dungeons
    , ascendStairsAtPlayerPosition
    , descendStairsAtPlayerPosition
    , exitDungeon
    , doPlayerAction
    , handleNpcTurn
    ) where

import           Control.Lens               ((%~), (&), (.~), (^.))
import           Control.Monad              (MonadPlus (mzero))
import           Control.Monad.Trans.Maybe  (MaybeT, mapMaybeT)
import           Control.Monad.Trans.Writer (Writer)
import           Coord                      (Coord)
import           Data.Foldable              (find)
import           Dungeon                    (Dungeon, actors, ascendingStairs,
                                             descendingStairs,
                                             positionOnParentMap, updateMap)
import qualified Dungeon                    as D
import           Dungeon.Actor              (Actor, isPlayer, position)
import           Dungeon.Actor.Actions      (Action)
import           Dungeon.Actor.Behavior     (npcAction)
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
                          (Just g, Just p, True) ->
                                Just $ modify (\d -> updateMap $ d & actors %~ (:) p) g
                          _ -> Nothing

descendStairsAtPlayerPosition :: Dungeons -> Maybe Dungeons
descendStairsAtPlayerPosition ds = newZipper
    where (player, zipperWithoutPlayer) = popPlayer ds
          newPlayer = case (player, newPosition) of
                          (Just p, Just pos) -> Just $ p & position .~ pos
                          _                  -> Nothing
          zipperFocusingNextDungeon = goDownBy (\x -> x ^. positionOnParentMap == fmap (^. position) player) zipperWithoutPlayer
          newPosition = downStairs <$> find (\(StairsPair from _) -> Just from == fmap (^. position) player) (getFocused ds ^. descendingStairs)
          newZipper = case (zipperFocusingNextDungeon, newPlayer) of
                          (Just g, Just p) -> Just $ modify (\d -> updateMap $ d & actors %~ (:) p) g
                          _ -> Nothing

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

doPlayerAction :: Action -> Dungeons -> MaybeT (Writer MessageLog) Dungeons
doPlayerAction action ds = result
    where (player, zipperWithoutPlayer) = popPlayer ds
          currentDungeonWithoutPlayer = getFocused zipperWithoutPlayer
          result = case player of
                       Just p  -> let dungeonAfterAction = action p currentDungeonWithoutPlayer
                                  in mapMaybeT (fmap (fmap (\d -> modify (const d) zipperWithoutPlayer))) dungeonAfterAction
                       Nothing -> mzero

handleNpcTurn :: Coord -> Dungeons -> MaybeT (Writer MessageLog) Dungeons
handleNpcTurn c ds = newDungeons
    where (theActor, dungeonsWithoutTheActor) = popActorAt c ds

          doAction actor = npcAction actor $ getFocused dungeonsWithoutTheActor

          newDungeon = maybe mzero doAction theActor

          newDungeons = mapMaybeT (fmap (fmap (\d -> modify (const d) dungeonsWithoutTheActor))) newDungeon

popPlayer :: Dungeons -> (Maybe Actor, Dungeons)
popPlayer = popActorIf isPlayer

popActorAt :: Coord -> Dungeons -> (Maybe Actor, Dungeons)
popActorAt c = popActorIf (\x -> x ^. position == c)

popActorIf :: (Actor -> Bool) -> Dungeons -> (Maybe Actor, Dungeons)
popActorIf f z = (fst $ D.popActorIf f $ getFocused z, modify (snd . D.popActorIf f) z)
