module KeenWM.Core
  ( KConfig(..)
  , Keyboard
  , Mouse
  , kToX
  , recompile
  , restart
  , recompileRestart
  , run
  , getScreens
  , getConfigDir
  ) where

import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Map as Map
import Data.Monoid (All(All))
import Debug.Trace (trace)
import qualified Graphics.X11.Types as X11
import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib (closeDisplay, openDisplay, rrScreenChangeNotifyMask)
import qualified Graphics.X11.Xlib.Cursor as X11
import Graphics.X11.Xlib.Extras (Event(RRScreenChangeNotifyEvent))
import Graphics.X11.Xrandr (xrrSelectInput)
import KeenWM.Bar (barLog, barStartup, cleanupBars)
import KeenWM.Util.ColorScheme (ColorScheme(..))
import KeenWM.Util.Dmenu (Dmenu)
import KeenWM.Util.Font (Font)
import KeenWM.Util.Terminal (Terminal(..), printToTerminal)
import KeenWM.Util.Xmobar (Xmobar)
import System.Directory (XdgDirectory(..), doesPathExist, getXdgDirectory)
import System.Environment (getProgName)
import System.Environment.Blank (getEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.Process
  ( CreateProcess
  , createProcess
  , cwd
  , proc
  , readCreateProcessWithExitCode
  , waitForProcess
  )
import Text.Printf (printf)
import qualified XMonad as X
import XMonad.Core (installSignalHandlers, uninstallSignalHandlers)
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.SetWMName (setWMName)
import qualified XMonad.Operations (restart)
import XMonad.Util.Cursor (setDefaultCursor)

--------------------------------------------------------------------------------
-- Configuration
--
type Keyboard a = KConfig a -> Map.Map (X11.KeyMask, X11.KeySym) (X.X ())

type Mouse a
   = KConfig a -> Map.Map (X11.ButtonMask, X11.Button) (X.Window -> X.X ())

data KConfig a =
  KConfig
    { colorScheme :: ColorScheme
    , dmenuConfig :: Dmenu
    , font :: Font
    , terminal :: Terminal
    , statusBars :: X.X [Xmobar]
    , layoutHook :: a X.Window
    , manageHook :: X.ManageHook
    , handleEventHook :: X.Event -> X.X All
    , workspaces :: [String]
    , modMask :: X11.KeyMask
    , keys :: Keyboard a
    , mouseBindings :: Mouse a
    , borderWidth :: X.Dimension
    , logHook :: X.X ()
    , startupHook :: X.X ()
    , focusFollowsMouse :: Bool
    , clickJustFocuses :: Bool
    }

-- | Translate an @KConfig@ instance into @XConfig@.
kToX :: KConfig a -> X.XConfig a
kToX kc@KConfig {colorScheme = cs} =
  X.def
    { X.normalBorderColor = base02 cs
    , X.focusedBorderColor = base0A cs
    , X.terminal = command $ terminal kc
    , X.workspaces = workspaces kc
    , X.borderWidth = borderWidth kc
    , X.focusFollowsMouse = focusFollowsMouse kc
    , X.clickJustFocuses = clickJustFocuses kc
    , X.modMask = modMask kc
    , X.keys = \_ -> keys kc kc
    , X.mouseBindings = \_ -> mouseBindings kc kc
    , X.layoutHook = layoutHook kc
    , X.manageHook = manageHook' `mappend` manageHook kc
    , X.handleEventHook = handleEventHook' `mappend` handleEventHook kc
    , X.logHook = logHook' >> logHook kc
    , X.startupHook = startupHook' >> startupHook kc
    }
  where
    manageHook' :: X.ManageHook
    manageHook' = manageDocks
    handleEventHook' :: X.Event -> X.X All
    handleEventHook' = randrEvent
    logHook' :: X.X ()
    logHook' = barLog
    startupHook' :: X.X ()
    startupHook' = do
      randrSetup
      setWMName "LG3D"
      setDefaultCursor X11.xC_left_ptr
      statusBars kc >>= barStartup

randrSetup :: X.X ()
randrSetup = do
  dpy <- X.asks X.display
  root <- X.asks X.theRoot
  liftIO $ xrrSelectInput dpy root rrScreenChangeNotifyMask

randrEvent :: X.Event -> X.X All
randrEvent e =
  case e of
    RRScreenChangeNotifyEvent {} -> restart >> return (All True)
    _ -> return $ All True

--------------------------------------------------------------------------------
-- Recompilation
--
-- | Recompile the project using stack and print the output
-- to the standard output if @Nothing@ is passed as a parameter.
-- Nothing is passed automatically by the keenwm executable
-- when used with the --recompile flag. When called from
-- a keybinding in the configuration it should always be passed
-- an instance of @Terminal@. That way if any errors occur, it will
-- spawn the sepecified terminal and display the error message.
-- If no error occurs, it does nothing else.
recompile :: Maybe Terminal -> IO Bool
recompile term = do
  configDir <- getConfigDir
  exists <- doesPathExist configDir
  uninstallSignalHandlers
  res <-
    if exists
      then maybe compileStdOut compileTerminal term
      else trace (printf "%s doesn't exist." configDir) return False
  installSignalHandlers
  return res
  where
    process :: FilePath -> CreateProcess
    process dir = (proc "stack" ["install"]) {cwd = Just dir}
    compileStdOut :: IO Bool
    compileStdOut = do
      configDir <- getConfigDir
      putStrLn $ printf "Recompiling the source code at %s." configDir
      (_, _, _, h) <- createProcess $ process configDir
      status <- waitForProcess h
      if status == ExitSuccess
        then trace "KeenWM recompilation exited with success." return True
        else trace "KeenWM recompilation exited with failure." return False
    compileTerminal :: Terminal -> IO Bool
    compileTerminal t = do
      configDir <- getConfigDir
      (status, _, err) <- readCreateProcessWithExitCode (process configDir) []
      let handleTerminal = do
            printToTerminal t err
            return False
      if status == ExitSuccess
        then return True
        else handleTerminal

-- | Restart keenwm.
restart :: X.X ()
restart =
  cleanupBars >> liftIO getProgName >>= (`XMonad.Operations.restart` True)

-- | Recompiles the project using the @recompile@ function
-- and restarts the instance of keenwm that it has been
-- called from if recompilation process is successful.
-- If any error occurs a terminal is spawned displaying
-- the error message by the recompile function. This
-- should be called by a keybinding.
recompileRestart :: Terminal -> X.X ()
recompileRestart t = liftIO (recompile . Just $ t) >>= (`when` restart)

--------------------------------------------------------------------------------
-- General utilities
--
-- | Like @spawn@ from XMonad, but it takes a @CreateProcess@ instance.
run :: MonadIO m => CreateProcess -> m ()
run = void . liftIO . createProcess

-- | Returns a list of screens.
getScreens :: MonadIO m => m [X.ScreenId]
getScreens =
  liftIO $ do
    screens <-
      do dpy <- openDisplay ""
         rects <- getScreenInfo dpy
         closeDisplay dpy
         return rects
    let ids = zip [0 ..] screens
    return $ map fst ids

-- | Returns the path to the keenwm source directory. @KEENWM_CONFIG@
-- environment variable is used to determine the path. If it does not
-- point to a valid path or not specified, then Xdg is used to determine.
-- Usually, it will be @$HOME\/.config\/keenwm@
getConfigDir :: IO FilePath
getConfigDir = do
  let xdg = getXdgDirectory XdgConfig "keenwm"
  envPath <- getEnv "KEENWM_CONFIG"
  case envPath of
    Nothing -> xdg
    Just p -> do
      envExists <- doesPathExist p
      if envExists
        then return p
        else xdg
