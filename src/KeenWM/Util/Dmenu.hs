{-# LANGUAGE LambdaCase #-}

module KeenWM.Util.Dmenu
  ( Dmenu(..)
  , dmenuDefaults
  , dmenuDefaults'
  , dmenu
  , dmenuRun
  ) where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromJust, isJust)
import KeenWM.Util.ColorScheme (ColorScheme(..))
import KeenWM.Util.Font (Font, getXft)
import System.Exit (ExitCode(..))
import System.IO (hClose, hFlush, hGetContents, hGetLine, hPutStr)
import System.Process
  ( StdStream(..)
  , createProcess
  , proc
  , readProcess
  , spawnCommand
  , spawnProcess
  , std_in
  , std_out
  , waitForProcess
  )

data Dmenu =
  Dmenu
    -- behaviour
    { dmenuFont :: Maybe Font
    , dmenuBottom :: Bool
    , dmenuCenter :: Bool
    , dmenuHidden :: Bool
    , dmenuIgnoreCase :: Bool
    , dmenuPrompt :: Maybe String
    , dmenuLines :: Maybe Integer
    , dmenuHeight :: Maybe Integer
    , dmenuBorderWidth :: Maybe Integer
    -- color
    , dmenuBackground :: Maybe String
    , dmenuForeground :: Maybe String
    , dmenuSelectedBackground :: Maybe String
    , dmenuSelectedForeground :: Maybe String
    }

dmenuDefaults :: Dmenu
dmenuDefaults =
  Dmenu
    { dmenuFont = Nothing
    , dmenuBottom = False
    , dmenuCenter = False
    , dmenuHidden = False
    , dmenuIgnoreCase = False
    , dmenuPrompt = Nothing
    , dmenuLines = Nothing
    , dmenuHeight = Nothing
    , dmenuBorderWidth = Nothing
    -- color
    , dmenuBackground = Nothing
    , dmenuForeground = Nothing
    , dmenuSelectedBackground = Nothing
    , dmenuSelectedForeground = Nothing
    }

dmenuDefaults' :: Font -> ColorScheme -> Dmenu
dmenuDefaults' fn cs =
  dmenuDefaults
    { dmenuFont = Just fn
    -- color
    , dmenuBackground = Just $ base00 cs
    , dmenuForeground = Just $ base05 cs
    , dmenuSelectedBackground = Just $ base08 cs
    , dmenuSelectedForeground = Just $ base07 cs
    }

constructArgs :: Dmenu -> [String]
constructArgs c =
  concat
    [ ["-b" | dmenuBottom c]
    , ["-c" | dmenuCenter c]
    , ["-P" | dmenuHidden c]
    , ["-i" | dmenuIgnoreCase c]
    , concat [["-fn", getXft $ fromJust $ dmenuFont c] | isJust $ dmenuFont c]
    , concat [["-p", fromJust $ dmenuPrompt c] | isJust $ dmenuPrompt c]
    , concat [["-l", show (fromJust $ dmenuLines c)] | isJust $ dmenuLines c]
    , concat [["-h", show (fromJust $ dmenuHeight c)] | isJust $ dmenuHeight c]
    , concat
        [ ["-bw", show (fromJust $ dmenuBorderWidth c)]
        | isJust $ dmenuBorderWidth c
        ]
    , concat
        [["-nb", fromJust $ dmenuBackground c] | isJust $ dmenuBackground c]
    , concat
        [["-nf", fromJust $ dmenuForeground c] | isJust $ dmenuForeground c]
    , concat
        [ ["-sb", fromJust $ dmenuSelectedBackground c]
        | isJust $ dmenuSelectedBackground c
        ]
    , concat
        [ ["-sf", fromJust $ dmenuSelectedForeground c]
        | isJust $ dmenuSelectedForeground c
        ]
    ]

dmenuRun :: MonadIO m => Dmenu -> m ()
dmenuRun c =
  liftIO $ do
    output <- readProcess "dmenu_path" [] []
    dmenu c (words output) $ \case
      Nothing -> return ()
      Just a -> do
        spawnCommand a
        return ()

dmenu :: MonadIO m => Dmenu -> [String] -> (Maybe String -> IO r) -> m r
dmenu c input action =
  liftIO $ do
    (Just hin, Just hout, _, h) <-
      createProcess
        (proc "dmenu" $ constructArgs c)
          {std_in = CreatePipe, std_out = CreatePipe}
    hPutStr hin $ unlines input
    hFlush hin
    hClose hin
    status <- waitForProcess h
    bracket
      (return hout)
      hClose
      (\pout ->
         if status == ExitSuccess
           then do
             output <- hGetLine pout
             action $ Just output
           else action Nothing)
