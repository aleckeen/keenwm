module KeenWM.Core
  ( KConfig(..)
  , Keyboard
  , Mouse
  , kToX
  , recompile
  , recompileRestart
  , run
  , getConfigDir
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Map as Map
import Data.Semigroup (All)
import Debug.Trace (trace)
import qualified Graphics.X11.Types as X11
import qualified Graphics.X11.Xlib.Cursor as X11
import KeenWM.Util.ColorScheme (ColorScheme(..))
import KeenWM.Util.Dmenu (Dmenu)
import KeenWM.Util.Font (Font)
import KeenWM.Util.Terminal (Terminal(..), printToTerminal)
import System.Directory
  ( XdgDirectory(..)
  , createDirectoryIfMissing
  , doesPathExist
  , getXdgDirectory
  )
import System.Environment (getProgName)
import System.Environment.Blank (getEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.IO (IOMode(..), hPutStrLn, withFile)
import System.Process
  ( CreateProcess
  , createProcess
  , cwd
  , proc
  , readCreateProcessWithExitCode
  , spawnProcess
  , waitForProcess
  )
import Text.Printf (printf)
import qualified XMonad as X
import XMonad.Core (installSignalHandlers, uninstallSignalHandlers)
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Operations (restart)
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
    , X.manageHook = manageHook'
    , X.handleEventHook = handleEventHook'
    , X.logHook = logHook'
    , X.startupHook = startupHook'
    }
  where
    manageHook' :: X.ManageHook
    manageHook' = manageDocks
    handleEventHook' :: X.Event -> X.X All
    handleEventHook' = mempty
    logHook' :: X.X ()
    logHook' = return ()
    startupHook' :: X.X ()
    startupHook' = do
      setWMName "LG3D"
      setDefaultCursor X11.xC_left_ptr

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

-- | Recompiles the project using the @recompile@ function
-- and restarts the instance of keenwm that it has been
-- called from if recompilation process is successful.
-- If any error occurs a terminal is spawned displaying
-- the error message by the recompile function. This
-- should be called by a keybinding.
recompileRestart :: Terminal -> X.X ()
recompileRestart t = do
  status <- liftIO $ recompile (Just t)
  when status $ do
    prog <- liftIO getProgName
    XMonad.Operations.restart prog True

--------------------------------------------------------------------------------
-- General utilities
--
-- | Like @spawn@ from XMonad, but it takes a @CreateProcess@ instance.
run :: MonadIO m => CreateProcess -> m ()
run p = do
  _ <- liftIO $ createProcess p
  return ()

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
