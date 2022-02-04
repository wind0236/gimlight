module Gimlight.Action.Melee
  ( meleeAction,
  )
where

import Control.Monad.State (StateT (runStateT))
import Control.Monad.Writer (runWriter, tell)
import Gimlight.Action
  ( Action,
    ActionResult (ActionResult),
    ActionStatus (Ok),
  )
import qualified Gimlight.Actor as A
import Gimlight.Dungeon.Map.Cell (locateActorAt, removeActorAt)
import Linear.V2 (V2)

meleeAction :: V2 Int -> Action
meleeAction offset srcPosition tc cm =
  case result of
    Right ((l, killed), newMap) -> do
      tell l
      return $ ActionResult Ok newMap killed
    _ -> error "Unreachable"
  where
    result =
      flip runStateT cm $ do
        attacker <- removeActorAt srcPosition
        defender <- removeActorAt dstPosition
        let ((newAttacker, newDefender), l') =
              runWriter $ A.attackFromTo attacker defender
        locateActorAt tc newAttacker srcPosition
        case newDefender of
          Just x -> do
            locateActorAt tc x dstPosition
            return (l', [])
          Nothing -> return (l', [defender])
    dstPosition = srcPosition + offset
