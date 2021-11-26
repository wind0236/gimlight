module GameStatus.Exploring.Dungeons
    ( Dungeons
    , ascendStairsAtPlayerPosition
    , descendStairsAtPlayerPosition
    , exitDungeon
    , doPlayerAction
    , handleNpcTurns
    ) where

import           Action                     (Action,
                                             ActionResult (killed, newDungeon, status),
                                             ActionStatus (Failed))
import           Actor                      (Actor, isPlayer, position)
import qualified Actor.NpcBehavior          as NPC
import           Control.Lens               ((&), (.~), (^.))
import           Control.Monad.Trans.Writer (Writer)
import           Data.Foldable              (find)
import           Data.Maybe                 (fromMaybe)
import           Dungeon                    (Dungeon, ascendingStairs,
                                             descendingStairs,
                                             positionOnParentMap, updateMap)
import qualified Dungeon                    as D
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
    newPlayer =
        case (player, newPosition) of
            (Just p, Just pos) -> Just $ p & position .~ pos
            _                  -> Nothing
    ascendable =
        (downStairs <$> getFocused ds ^. ascendingStairs) ==
        fmap (^. position) player
    zipperFocusingNextDungeon = goUp zipperWithoutPlayer
    newPosition = upStairs <$> getFocused ds ^. ascendingStairs
    newZipper =
        case (zipperFocusingNextDungeon, newPlayer, ascendable) of
            (Just g, Just p, True) -> updateMapOrError g p
            _                      -> Nothing
    updateMapOrError g p =
        Just $
        modify
            (\d ->
                 fromMaybe
                     (error "Failed to update the map.")
                     (updateMap ts $ D.pushActor p d))
            g

descendStairsAtPlayerPosition :: TileCollection -> Dungeons -> Maybe Dungeons
descendStairsAtPlayerPosition ts ds = newZipper
  where
    (player, zipperWithoutPlayer) = popPlayer ds
    newPlayer =
        case (player, newPosition) of
            (Just p, Just pos) -> Just $ p & position .~ pos
            _                  -> Nothing
    zipperFocusingNextDungeon =
        goDownBy
            (\x -> x ^. positionOnParentMap == fmap (^. position) player)
            zipperWithoutPlayer
    newPosition =
        downStairs <$>
        find
            (\(StairsPair from _) -> Just from == fmap (^. position) player)
            (getFocused ds ^. descendingStairs)
    newZipper =
        case (zipperFocusingNextDungeon, newPlayer) of
            (Just g, Just p) -> updateMapOrError g p
            _                -> Nothing
    updateMapOrError g p =
        Just $
        modify
            (\d ->
                 fromMaybe
                     (error "Failed to update the map.")
                     (updateMap ts $ D.pushActor p d))
            g

exitDungeon :: Dungeons -> Maybe Dungeons
exitDungeon ds = newZipper
  where
    (player, zipperWithoutPlayer) = popPlayer ds
    currentDungeon = getFocused ds
    newPosition = currentDungeon ^. positionOnParentMap
    newPlayer =
        case (player, newPosition) of
            (Just p, Just pos) -> Just $ p & position .~ pos
            _                  -> Nothing
    zipperFocusingGlobalMap = goUp zipperWithoutPlayer
    newZipper =
        case (zipperFocusingGlobalMap, newPlayer) of
            (Just g, Just p) -> Just $ modify (D.pushActor p) g
            _                -> Nothing

doPlayerAction ::
       Action
    -> TileCollection
    -> Dungeons
    -> Writer MessageLog (ActionStatus, Dungeons, [Actor])
doPlayerAction action ts ds = result
  where
    (player, zipperWithoutPlayer) = popPlayer ds
    currentDungeonWithoutPlayer = getFocused zipperWithoutPlayer
    result =
        case player of
            Just p -> do
                actionResult <- action p ts currentDungeonWithoutPlayer
                let statusAndNewDungeon =
                        (status actionResult, newDungeon actionResult)
                return $
                    (\(a, d) ->
                         ( a
                         , modify (const d) zipperWithoutPlayer
                         , killed actionResult))
                        statusAndNewDungeon
            Nothing -> return (Failed, ds, [])

handleNpcTurns ::
       TileCollection -> Dungeons -> Writer MessageLog (Dungeons, [Actor])
handleNpcTurns ts ds =
    (\(x, ks) -> (modify (const x) ds, ks)) <$>
    NPC.handleNpcTurns ts (getFocused ds)

popPlayer :: Dungeons -> (Maybe Actor, Dungeons)
popPlayer = popActorIf isPlayer

popActorIf :: (Actor -> Bool) -> Dungeons -> (Maybe Actor, Dungeons)
popActorIf f z =
    (fst $ D.popActorIf f $ getFocused z, modify (snd . D.popActorIf f) z)
