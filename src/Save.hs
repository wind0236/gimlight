module Save
    ( save
    , load
    ) where

import           Data.Binary      (decodeFile, encodeFile)
import           GameModel.Status (GameStatus)

save :: GameStatus -> IO ()
save = encodeFile saveFile

load :: IO GameStatus
load = decodeFile saveFile

saveFile :: String
saveFile = "save"
