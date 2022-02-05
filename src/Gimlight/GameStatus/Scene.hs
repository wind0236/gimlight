{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Gimlight.GameStatus.Scene
  ( SceneHandler,
    sceneHandler,
    withoutSpeaker,
    text,
    getBackgroundImagePath,
    getCurrentScene,
    nextSceneOrFinish,
  )
where

import Control.Lens (makeLenses, view, (%~), (&), (^.))
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)
import Gimlight.GameStatus.Exploring (ExploringHandler)
import Gimlight.Localization (MultilingualText)

newtype SceneElement
  = WithoutSpeaker MultilingualText
  deriving (Show, Ord, Eq, Generic)

instance Binary SceneElement

data SceneHandler = SceneHandler
  { _backgroundImage :: Text,
    _elements :: [SceneElement],
    _afterScene :: ExploringHandler
  }
  deriving (Show, Ord, Eq, Generic)

makeLenses ''SceneHandler

instance Binary SceneHandler

withoutSpeaker :: MultilingualText -> SceneElement
withoutSpeaker = WithoutSpeaker

text :: SceneElement -> MultilingualText
text (WithoutSpeaker t) = t

getBackgroundImagePath :: SceneHandler -> Text
getBackgroundImagePath = view backgroundImage

getCurrentScene :: SceneHandler -> SceneElement
getCurrentScene = head . view elements

sceneHandler :: Text -> [SceneElement] -> ExploringHandler -> SceneHandler
sceneHandler = SceneHandler

nextSceneOrFinish :: SceneHandler -> Either ExploringHandler SceneHandler
nextSceneOrFinish sh
  | length (sh ^. elements) == 1 = Left $ sh ^. afterScene
  | otherwise = Right $ sh & elements %~ tail
