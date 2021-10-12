module Save
    ( save
    , load
    ) where

import           Data.Binary (decodeFile, encodeFile)
import           Engine      (Engine)

save :: Engine -> IO ()
save = encodeFile saveFile

load :: IO Engine
load = decodeFile saveFile

saveFile :: String
saveFile = "save"
