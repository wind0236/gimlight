module Scene
    ( Scene
    , SceneElement(..)
    , withoutSpeaker
    , gameStartScene
    ) where

type Scene = [SceneElement]

data SceneElement = WithoutSpeaker String | WithSpeaker { name    :: String
                                                        , message :: String
                                                        } deriving (Show)
withSpeaker :: String -> String -> SceneElement
withSpeaker = WithSpeaker

withoutSpeaker :: String -> SceneElement
withoutSpeaker = WithoutSpeaker

gameStartScene :: Scene
gameStartScene = [ withoutSpeaker "This story is about a stupid man, Ruskel. His most significant characteristic is that he has a strong stomach, so he can eat a lump of raw meat and does not end up with diarrhea."
                 , withSpeaker "Ruskel" "I want to eat delicious foods"
                 , withoutSpeaker "...His actions are based on this simple desire."
                 ]
