{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module KeenWM.Config
  ( Default(..)
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bits ((.|.))
import Data.Default (Default)
import qualified Data.Map as Map
import qualified Graphics.X11.Types as X11
import KeenWM.Core (Keyboard, Mouse, barForEachScreen, recompileRestart, run)
import qualified KeenWM.Core as K (KConfig(..))
import KeenWM.Util.ColorScheme (ColorScheme, snazzyCS)
import KeenWM.Util.Dmenu (dmenuDefaults', dmenuRun)
import KeenWM.Util.Font (Font, fontDefaults)
import KeenWM.Util.Terminal (Terminal(..), alacritty)
import KeenWM.Util.Xmobar (Xmobar(template), xmobarDefaults)
import System.Exit (exitSuccess)
import System.Process (shell)
import qualified XMonad as X
import qualified XMonad.StackSet as W

instance (a ~ X.Choose X.Tall (X.Choose (X.Mirror X.Tall) X.Full)) =>
         Default (K.KConfig a) where
  def =
    K.KConfig
      { K.colorScheme = colorScheme
      , K.dmenuConfig = dmenuDefaults' font colorScheme
      , K.font = font
      , K.terminal = terminal
      , K.statusBars = statusBars
      , K.workspaces = map show [1 .. 9 :: Int]
      , K.borderWidth = 1
      , K.focusFollowsMouse = True
      , K.clickJustFocuses = True
      , K.modMask = X11.mod4Mask
      , K.keys = keyboard
      , K.mouseBindings = mouse
      , K.layoutHook = X.layoutHook X.def
      , K.manageHook = X.manageHook X.def
      , K.handleEventHook = mempty
      , K.logHook = return ()
      , K.startupHook = return ()
      }

colorScheme :: ColorScheme
colorScheme = snazzyCS

font :: Font
font = fontDefaults

terminal :: Terminal
terminal = alacritty

statusBars :: X.X [Xmobar]
statusBars = barForEachScreen bar
  where
    bar :: Xmobar
    bar = xmobarDefaults {template = "%WMReader%"}

keyboard :: Keyboard a
keyboard K.KConfig { K.modMask = modm
                   , K.terminal = term
                   , K.dmenuConfig = dmenu
                   , K.workspaces = ws
                   } =
  Map.fromList $
  [ ( (modm .|. X11.controlMask .|. X11.shiftMask, X11.xK_r)
    , recompileRestart term)
  , ((modm .|. X11.controlMask .|. X11.shiftMask, X11.xK_q), liftIO exitSuccess)
  , ((modm, X11.xK_r), dmenuRun dmenu)
  , ((modm, X11.xK_Return), run . shell $ command term)
  ] ++
  [ ((m .|. modm, k), X.windows $ f i)
  | (i, k) <- zip ws [X11.xK_1 ..]
  , (f, m) <- [(W.greedyView, 0), (W.shift, X11.shiftMask)]
  ]

mouse :: Mouse a
mouse K.KConfig {K.modMask = modm} =
  Map.fromList
    [ ( (modm, X11.button1)
      , \w -> X.focus w >> X.mouseMoveWindow w >> X.windows W.shiftMaster)
    , ((modm, X11.button2), X.windows . (W.shiftMaster .) . W.focusWindow)
    , ( (modm, X11.button3)
      , \w -> X.focus w >> X.mouseResizeWindow w >> X.windows W.shiftMaster)
    ]
