module Event
    ( Event
    , withoutSpeaker
    , popMessage
    , gameStartEvent
    , numMessages
    ) where

data Event = WithoutSpeaker
           { messages :: [String]
           }
           | WithSpeaker
           { speakerAndMessages :: [(String, String)]
           } deriving (Show)

withoutSpeaker :: [String] -> Event
withoutSpeaker messages = WithoutSpeaker { messages = messages }

numMessages :: Event -> Int
numMessages WithoutSpeaker { messages = messages }        = length messages
numMessages WithSpeaker { speakerAndMessages = messages } = length messages

popMessage :: Event -> (Maybe String, Event)
popMessage e@WithoutSpeaker { messages = [] } = (Nothing, e)
popMessage WithoutSpeaker { messages = messages } = (Just $ head messages, WithoutSpeaker { messages = tail messages })
popMessage e@WithSpeaker { speakerAndMessages = [] } = (Nothing, e)
popMessage WithSpeaker { speakerAndMessages = messages } = (Just $ snd $ head messages, WithSpeaker { speakerAndMessages = tail messages })

gameStartEvent :: Event
gameStartEvent = withoutSpeaker [ "On the planet, which is similar to the earth because its environment is suitable for the living things, but not identical because there are creatures we think are fictional, there is a country. While the government does not govern all regions perfectly, the controlled areas are like a dystopia. The government constitutes an army and is trying to extend its controlling regions."
                                , "BY THE WAY, there is a stupid man in the country. His home is far from the controlled district, so he even does not know the nation's actual situation. The most significant characteristic of the man is that he has a strong stomach, so he can eat a lump of raw meat and does not end up with diarrhea."
                                ]