module KeenWM.Util.Terminal
  ( Terminal(..)
  , alacritty
  , printToTerminal
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import System.Process (spawnProcess)
import Text.Printf (printf)

data Terminal =
  Terminal
    { command :: String
    , classFlag :: String
    , commandFlag :: String
    }

alacritty :: Terminal
alacritty =
  Terminal {command = "alacritty", classFlag = "--class", commandFlag = "-e"}

printToTerminal :: MonadIO m => Terminal -> String -> m ()
printToTerminal t s =
  liftIO $ do
    temp <- getTemporaryDirectory
    let file = temp </> "keenwm-terminal"
    writeFile file s
    liftIO $
      spawnProcess
        (command t)
        [ commandFlag t
        , "sh"
        , "-c"
        , printf "cat %s && read _ && rm %s " file file
        ]
    return ()
