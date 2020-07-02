module KeenWM.Bar
  ( barStartup
  , barLog
  , cleanupBars
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.Chan (Chan, newChan, writeChan)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (fromJust, isJust)
import KeenWM.Util.Xmobar (Xmobar(focusedPP, normalPP, readWM), xmobarToConfig)
import XMonad
  ( ExtensionClass(..)
  , Typeable
  , WorkspaceId
  , X
  , get
  , screenWorkspace
  , windowset
  )
import XMonad.Hooks.DynamicLog (PP, dynamicLogString)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import Xmobar (xmobar)

newtype BarInfo =
  BarInfo
    { dsbInfo :: [(Xmobar, Chan String, ThreadId)]
    }
  deriving (Typeable)

instance ExtensionClass BarInfo where
  initialValue = BarInfo []

barStartup :: [Xmobar] -> X ()
barStartup bars = start >>= XS.put . BarInfo
  where
    start :: X [(Xmobar, Chan String, ThreadId)]
    start =
      liftIO . forM bars $ \c -> do
        chan <- newChan
        tid <- forkIO . xmobar $ xmobarToConfig c chan
        return (c, chan, tid)

barLog :: X ()
barLog = do
  dsb <- dsbInfo <$> XS.get
  barLog' dynamicLogString $ filter (\(c, _, _) -> isJust . readWM $ c) dsb

barLog' :: (PP -> X String) -> [(Xmobar, Chan String, ThreadId)] -> X ()
barLog' dynlStr bars = do
  st <- get
  let writeBar :: Xmobar -> WorkspaceId -> Chan String -> X ()
      writeBar c ws chan = do
        out <-
          dynlStr $
          if (ws ==) . W.tag . W.workspace . W.current $ windowset st
            then focusedPP c
            else normalPP c
        liftIO . writeChan chan $ out
  forM_ bars $ \(c, chan, _) -> do
    ws <- screenWorkspace . fromJust . readWM $ c
    case ws of
      Nothing -> return ()
      Just w -> writeBar c w chan

cleanupBars :: X ()
cleanupBars = do
  (_, _, tids) <- unzip3 . dsbInfo <$> XS.get
  liftIO . forM_ tids $ killThread
