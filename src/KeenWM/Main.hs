{-# LANGUAGE FlexibleContexts #-}

module KeenWM.Main
  ( keenwm
  ) where

import Control.Monad (unless)
import Data.Version (showVersion)
import Graphics.X11.Xinerama (compiledWithXinerama)
import Graphics.X11.Xlib
  ( allocaXEvent
  , clientMessage
  , defaultScreen
  , internAtom
  , openDisplay
  , rootWindow
  , sendEvent
  , structureNotifyMask
  , sync
  )
import Graphics.X11.Xlib.Extras
  ( currentTime
  , setClientMessageEvent
  , setEventType
  )
import KeenWM.Core (KConfig, getConfigDir, kToX, recompile)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.Info (arch, compilerName, compilerVersion, os)
import Text.Printf (printf)
import qualified XMonad as X

version :: String
version = "0.1"

keenwm ::
     (X.LayoutClass l X.Window, Read (l X.Window))
  => KConfig l
  -> (X.XConfig l -> X.XConfig l)
  -> IO ()
keenwm conf m = do
  args <- getArgs
  case args of
    ["--help"] -> printUsage
    ["--recompile"] -> recompile Nothing >>= flip unless exitFailure
    ["--restart"] -> sendRestart
    ["--info"] -> printInfo
    [] -> X.launch . m . kToX $ conf
    a -> printUnrecognized a

printInfo :: IO ()
printInfo = do
  configPath <- getConfigDir
  putStrLn $
    printf
      "keenwm %s compiled by %s %s for %s-%s\n\
      \Xinerama: %s\n\
      \Config path: %s"
      version
      compilerName
      (showVersion compilerVersion)
      arch
      os
      (show compiledWithXinerama)
      configPath

printUsage :: IO ()
printUsage = do
  self <- getProgName
  dir <- getConfigDir
  putStrLn $
    printf
      "Usage: %s [OPTION]\n\
      \Options:\n\
      \  --help        Print this message\n\
      \  --recompile   Recompile the source code at %s\n\
      \  --restart     Restart the running instance of keenwm (Broken)\n\
      \  --info        Print info about the binary"
      self
      dir

printUnrecognized :: [String] -> IO ()
printUnrecognized o = do
  self <- getProgName
  putStrLn $
    printf
      "%s: unrecognized option%s -- '%s'\n\
      \Try '%s --help' for more information."
      self
      (if length o == 1
         then ""
         else "s")
      (unwords o)
      self

-- | Broken. Starts xmonad instead of keenwm
sendRestart :: IO ()
sendRestart = do
  dpy <- openDisplay ""
  rw <- rootWindow dpy $ defaultScreen dpy
  xmonad_restart <- internAtom dpy "XMONAD_RESTART" False
  allocaXEvent $ \e -> do
    setEventType e clientMessage
    setClientMessageEvent e rw xmonad_restart 32 0 currentTime
    sendEvent dpy rw False structureNotifyMask e
  sync dpy False
