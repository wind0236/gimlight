{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Quest
    ( QuestCollection
    , Inquiry(..)
    , Updater(..)
    , questCollection
    , handleWithTurnResult
    , inquiry
    , update
    ) where

import qualified Actor.Identifier   as A
import           Control.Lens       (makeLenses, (%~), (&), (.~), (^.))
import           Data.Binary        (Binary)
import qualified Dungeon.Identifier as D
import           GHC.Generics       (Generic)
import           Quest.KillBats     (KillBats)
import qualified Quest.KillBats     as KillBats

data Inquiry
    = IsKillBatsStarted
    | IsEnoughBatsKilled
    | IsKillBatsCompleted
    deriving (Show, Ord, Eq, Generic)

instance Binary Inquiry

data Updater
    = StartKillBats
    | CompleteKillBats
    deriving (Show, Ord, Eq, Generic)

instance Binary Updater

newtype QuestCollection =
    QuestCollection
        { _killBats :: KillBats
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''QuestCollection

instance Binary QuestCollection

questCollection :: QuestCollection
questCollection = QuestCollection {_killBats = KillBats.killBats}

handleWithTurnResult ::
       D.Identifier -> [A.Identifier] -> QuestCollection -> QuestCollection
handleWithTurnResult currentDungeon killed qc =
    qc & killBats %~ KillBats.handleWithTurnResult currentDungeon killed

inquiry :: Inquiry -> QuestCollection -> Bool
inquiry IsKillBatsStarted qc   = qc ^. killBats & KillBats.isQuestStarted
inquiry IsEnoughBatsKilled qc  = qc ^. killBats & KillBats.isEnoughBatsKilled
inquiry IsKillBatsCompleted qc = qc ^. killBats & KillBats.isQuestCompleted

update :: Updater -> QuestCollection -> Maybe QuestCollection
update StartKillBats qc =
    (\x -> qc & killBats .~ x) <$> KillBats.startQuest (qc ^. killBats)
update CompleteKillBats qc =
    (\x -> qc & killBats .~ x) <$> KillBats.completeQuest (qc ^. killBats)
