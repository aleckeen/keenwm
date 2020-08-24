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
import Paths_keenwm (version)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.Info (arch, compilerName, compilerVersion, os)
import Text.Printf (printf)
import qualified XMonad as X

keenwm ::
     (X.LayoutClass l X.Window, Read (l X.Window))
  => (X.XConfig l -> X.XConfig l)
  -> KConfig l
  -> IO ()
keenwm m conf = do
  args <- getArgs
  case args of
    ["--help"] -> printUsage
    ["-h"] -> printUsage
    ["--version"] -> printVersion
    ["-v"] -> printVersion
    ["-vv"] -> printInfo
    ["--src-dir"] -> printConfigDir
    ["-s"] -> printConfigDir
    ["--recompile"] -> runRecompile
    ["-r"] -> runRecompile
    ["--restart"] -> runRestart
    ["-e"] -> runRestart
    [] -> runKeenWM m conf
    a -> do
      putStrLn . printf "Unrecognized options: %s\n" $ unwords a
      printUsage
      exitFailure

printVersion :: IO ()
printVersion = putStrLn . printf "KeenWM: %s" $ showVersion version

printInfo :: IO ()
printInfo = do
  configPath <- getConfigDir
  putStrLn $
    printf
      "keenwm %s compiled by %s %s for %s-%s\n\
      \Xinerama: %s\n\
      \Config path: %s"
      (showVersion version)
      compilerName
      (showVersion compilerVersion)
      arch
      os
      (show compiledWithXinerama)
      configPath

printUsage :: IO ()
printUsage = do
  self <- getProgName
  putStrLn $
    printf
      "KeenWM %s\n\
      \A window manager written in Haskell\n\
      \\n\
      \USAGE:\n\
      \    %s [FLAGS]\n\
      \\n\
      \FLAGS:\n\
      \    -h, --help        Prints help information\n\
      \    -v, --version     Prints version information\n\
      \    -s, --src-dir     Prints the source code path\n\
      \    -r, --recompile   Recompiles the source code\n\
      \    -e, --restart     Requests a running KeenWM process to restart\n"
      (showVersion version)
      self

printConfigDir :: IO ()
printConfigDir = putStrLn =<< getConfigDir

runRecompile :: IO ()
runRecompile = recompile Nothing >>= flip unless exitFailure

runRestart :: IO ()
runRestart = sendRestart

runKeenWM ::
     (X.LayoutClass l X.Window, Read (l X.Window))
  => (X.XConfig l -> X.XConfig l)
  -> KConfig l
  -> IO ()
runKeenWM m = X.launch . m . kToX

sendRestart :: IO ()
sendRestart = do
  dpy <- openDisplay ""
  rw <- rootWindow dpy $ defaultScreen dpy
  keenwm_restart <- internAtom dpy "KEENWM_RESTART" False
  allocaXEvent $ \e -> do
    setEventType e clientMessage
    setClientMessageEvent e rw keenwm_restart 32 0 currentTime
    sendEvent dpy rw False structureNotifyMask e
  sync dpy False
