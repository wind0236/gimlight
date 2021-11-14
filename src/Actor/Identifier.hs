{-# LANGUAGE DeriveGeneric #-}

module Actor.Identifier
    ( Identifier(..)
    , toName
    ) where

import           Data.Binary        (Binary)
import           GHC.Generics       (Generic)
import           Localization       (MultilingualText)
import qualified Localization.Texts as T

data Identifier
    = Orc
    | Troll
    | Electria
    | Player
    deriving (Show, Ord, Eq, Generic)

instance Binary Identifier

toName :: Identifier -> MultilingualText
toName Orc      = T.orc
toName Troll    = T.troll
toName Electria = T.electria
toName Player   = T.player
