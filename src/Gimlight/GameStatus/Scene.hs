{-# LANGUAGE DeriveGeneric #-}

module Gimlight.GameStatus.Scene
    ( SceneHandler
    , sceneHandler
    , withoutSpeaker
    , text
    , getBackgroundImagePath
    , getCurrentScene
    , nextSceneOrFinish
    ) where

import           Data.Binary                   (Binary)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           Gimlight.GameStatus.Exploring (ExploringHandler)
import           Gimlight.Localization         (MultilingualText)

newtype SceneElement =
    WithoutSpeaker MultilingualText
    deriving (Show, Ord, Eq, Generic)

instance Binary SceneElement

data SceneHandler =
    SceneHandler
        { backgroundImage :: Text
        , elements        :: [SceneElement]
        , afterScene      :: ExploringHandler
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary SceneHandler

withoutSpeaker :: MultilingualText -> SceneElement
withoutSpeaker = WithoutSpeaker

text :: SceneElement -> MultilingualText
text (WithoutSpeaker t) = t

getBackgroundImagePath :: SceneHandler -> Text
getBackgroundImagePath = backgroundImage

getCurrentScene :: SceneHandler -> SceneElement
getCurrentScene = head . elements

sceneHandler :: Text -> [SceneElement] -> ExploringHandler -> SceneHandler
sceneHandler = SceneHandler

nextSceneOrFinish :: SceneHandler -> Either ExploringHandler SceneHandler
nextSceneOrFinish SceneHandler { backgroundImage = b
                               , elements = es
                               , afterScene = af
                               } =
    if length es == 1
        then Left af
        else Right $
             SceneHandler
                 {backgroundImage = b, elements = tail es, afterScene = af}
