module Gimlight.Action.Consume
  ( consumeAction,
  )
where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.State (StateT (runStateT), execStateT)
import Control.Monad.Writer (tell)
import Gimlight.Action
  ( Action,
    ActionResult (ActionResult),
    ActionResultWithLog,
    ActionStatus (Failed, Ok, ReadingStarted),
  )
import Gimlight.Actor
  ( Actor,
    getIdentifier,
    healHp,
    inventoryItems,
  )
import Gimlight.Actor.Identifier (toName)
import Gimlight.Dungeon.Map.Cell
  ( CellMap,
    locateActorAt,
    removeActorAt,
  )
import Gimlight.Inventory (removeNthItem)
import Gimlight.Item
  ( Effect (Book, Heal),
    getEffect,
    isUsableManyTimes,
  )
import Gimlight.Item.Heal (getHealAmount)
import qualified Gimlight.Localization.Texts as T

consumeAction :: Int -> Action
consumeAction n position tc cm =
  case flip runStateT cm $ removeActorAt position of
    Right (a, ncm) -> consumeActionForActor a ncm
    _ -> error "No such actor."
  where
    consumeActionForActor actorWithItem ncm =
      case removeNthItem n (actorWithItem ^. inventoryItems) of
        (Just x, i) ->
          useItem
            x
            (actorWithItem & inventoryItems .~ i)
            actorWithItem
            ncm
        (Nothing, _) -> do
          tell [T.whatToUse]
          return $ ActionResult Failed cm []
    useItem x actorWithoutItem actorWithItem ncm =
      let actor =
            if isUsableManyTimes x
              then actorWithItem
              else actorWithoutItem
       in doItemEffect (getEffect x) actor ncm
    doItemEffect :: Effect -> Actor -> CellMap -> ActionResultWithLog
    doItemEffect (Heal handler) a ncm = do
      tell [T.healed (toName $ getIdentifier a) amount]
      return $ ActionResult Ok cmAfterHealing []
      where
        amount = getHealAmount handler
        cmAfterHealing =
          case flip execStateT ncm $
            locateActorAt tc (healHp amount a) position of
            Right x -> x
            Left e -> error $ "Failed to locate an actor." <> show e
    doItemEffect (Book handler) a ncm =
      return $ ActionResult (ReadingStarted handler) cmAfterReading []
      where
        cmAfterReading =
          case flip execStateT ncm $ locateActorAt tc a position of
            Right x -> x
            Left e -> error $ "Failed to locate an actor." <> show e
