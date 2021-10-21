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
import           Localization (MultilingualText, multilingualText)

newtype SceneElement = WithoutSpeaker MultilingualText deriving (Show, Eq, Ord, Generic)
instance Binary SceneElement

data Scene = Scene
    { _backgroundImage :: Text
    , _elements        :: [SceneElement]
    } deriving (Show, Ord, Eq, Generic)
makeLenses ''Scene
instance Binary Scene

withoutSpeaker :: MultilingualText -> SceneElement
withoutSpeaker = WithoutSpeaker

text :: SceneElement -> MultilingualText
text (WithoutSpeaker t) = t

gameStartScene :: Scene
gameStartScene = Scene
    { _backgroundImage = "images/game_opening.png"
    , _elements = xs
    }
    where xs = [ withoutSpeaker $ multilingualText "This is the English text 1." "これは日本語テキスト1です．"
               , withoutSpeaker $ multilingualText "And this is the English text 2." "そしてこれは日本語テキスト2です．"
               ]
