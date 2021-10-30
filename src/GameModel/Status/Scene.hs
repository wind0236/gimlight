{-# LANGUAGE DeriveGeneric #-}
module GameModel.Status.Scene
    ( SceneHandler
    , sceneHandler
    , destructHandler
    , nextSceneOrFinish
    ) where
import           Control.Lens               ((%~), (&), (^.))
import           Data.Binary                (Binary)
import           GHC.Generics               (Generic)
import           GameModel.Status.Exploring (ExploringHandler)
import           Scene                      (Scene, elements)

data SceneHandler = SceneHandler
                  { scene      :: Scene
                  , afterScene :: ExploringHandler
                  } deriving (Show, Ord, Eq, Generic)

instance Binary SceneHandler

sceneHandler :: Scene -> ExploringHandler -> SceneHandler
sceneHandler = SceneHandler

destructHandler :: SceneHandler -> (Scene, ExploringHandler)
destructHandler SceneHandler { scene = s, afterScene = after } = (s, after)

nextSceneOrFinish :: SceneHandler -> Either ExploringHandler SceneHandler
nextSceneOrFinish SceneHandler { scene = s, afterScene = af } =
    if length (s ^. elements) == 1
        then Left af
        else Right $ SceneHandler { scene = s & elements %~ tail, afterScene = af }
