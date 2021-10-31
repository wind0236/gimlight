{-# LANGUAGE DeriveGeneric #-}

module Dungeon.Actor.Status
    ( Status
    , status
    , getLevel
    , getHp
    , getMaxHp
    , getCurrentExperiencePoint
    , getExperiencePointForNextLevel
    , attackFromTo
    , healHp
    , getPower
    , getDefence
    ) where

import           Data.Binary                     (Binary)
import           Data.Maybe                      (isNothing)
import           Dungeon.Actor.Status.Experience (Experience, gainExperience)
import qualified Dungeon.Actor.Status.Experience as E
import           Dungeon.Actor.Status.Hp         (Hp)
import qualified Dungeon.Actor.Status.Hp         as HP
import           GHC.Generics                    (Generic)
import           Localization                    (MultilingualText)
import qualified Localization.Texts              as T
import           Log                             (MessageLog)
import qualified Log                             as M

data Status = Status
            { hp         :: Hp
            , power      :: Int
            , defence    :: Int
            , experience :: Experience
            } deriving (Show, Ord, Eq, Generic)

instance Binary Status

status :: Hp -> Int -> Int -> Status
status h p d = Status h p d E.experience

getHp :: Status -> Int
getHp Status { hp = h }= HP.getHp h

getMaxHp :: Status -> Int
getMaxHp Status { hp = h }= HP.getMaxHp h

attackFromTo :: Status -> Status -> (Status, Maybe Status, MultilingualText -> MultilingualText -> MessageLog)
attackFromTo attacker defender = (newAttacker, newDefender, message)
    where damage = max 0 $ getPower attacker - getDefence defender

          newAttacker = attacker { power = getPower attacker + levelUp
                                 , defence = getDefence attacker + levelUp
                                 , experience = newAttackerExp
                                 }

          newDefender = receiveDamage damage defender

          (levelUp, newAttackerExp) = if isNothing newDefender
                                       then gainExperience experiencePointAmount (experience attacker)
                                       else (0, experience attacker)

          experiencePointAmount = getPower defender + getDefence defender

          message =
            case newDefender of
                        Just _  -> notKilledMessage
                        Nothing -> killedMessage

          notKilledMessage = \a d ->
            if damage > 0
                then [M.message $ T.damagedMessage damage a d]
                else [M.message $ T.noDamageMessage a d]

          killedMessage =
            \a d -> map M.message [T.damagedMessage damage a d, T.deathMessage d]
                     ++ [M.message $ T.levelUp a (getLevel newAttacker) | levelUp > 0]

healHp :: Int -> Status -> Status
healHp amount a@Status { hp = h } =
    a { hp = HP.healHp amount h }

receiveDamage :: Int -> Status -> Maybe Status
receiveDamage damage a@Status { hp = h } =
    (\x -> a { hp = x }) <$> HP.receiveDamage damage h

getPower :: Status -> Int
getPower = power

getDefence :: Status -> Int
getDefence = defence

getLevel :: Status -> Int
getLevel Status { experience = e } = E.getLevel e

getCurrentExperiencePoint :: Status -> Int
getCurrentExperiencePoint Status { experience = e } = E.getCurrentExperiencePoint e

getExperiencePointForNextLevel :: Status -> Int
getExperiencePointForNextLevel Status { experience = e } = E.pointForNextLevel e
