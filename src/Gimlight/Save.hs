module Gimlight.Save
  ( save,
    load,
  )
where

import Data.Binary (decodeFile, encodeFile)
import Gimlight.GameStatus (GameStatus)

save :: GameStatus -> IO ()
save = encodeFile saveFile

load :: IO GameStatus
load = decodeFile saveFile

saveFile :: String
saveFile = "save"
