{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Scene
    ( Scene
    , SceneElement(..)
    , withoutSpeaker
    , gameStartScene
    , backgroundImage
    , elements
    , text
    ) where

import           Control.Lens (makeLenses)
import           Data.Binary  (Binary)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

newtype SceneElement = WithoutSpeaker Text deriving (Show, Eq, Ord, Generic)
instance Binary SceneElement

data Scene = Scene
    { _backgroundImage :: Text
    , _elements        :: [SceneElement]
    } deriving (Show, Ord, Eq, Generic)
makeLenses ''Scene
instance Binary Scene

withoutSpeaker :: Text -> SceneElement
withoutSpeaker = WithoutSpeaker

text :: SceneElement -> Text
text (WithoutSpeaker t) = t

gameStartScene :: Scene
gameStartScene = Scene
    { _backgroundImage = "images/game_opening.png"
    , _elements = xs
    }
    where xs = [ withoutSpeaker "Planet Kantsarta is a strange planet. There lives not only human, animals, and plants, but also therianthropies, fairies, undeads, and even dragons."
               , withoutSpeaker "And you will leave from Beaeve, a country town located in one of countries existing on the planet, Karpho. Without knowing what will happen in the future. Having a great hope."
               ]
