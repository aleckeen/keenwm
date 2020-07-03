module KeenWM.Util.Xmobar
  ( Xmobar(..)
  , xmobarToConfig
  , xmobarDefaults
  , screen
  ) where

import Control.Concurrent.Chan (Chan, readChan)
import Control.Monad (forever)
import KeenWM.Util.ColorScheme (ColorScheme(..), snazzyCS)
import KeenWM.Util.Font (Font, fontDefaults, getXft)
import XMonad (ScreenId(S))
import XMonad.Hooks.DynamicLog (PP, xmobarPP)
import Xmobar
  ( Border(NoBorder)
  , Config(Config)
  , Exec(alias, start)
  , Runnable(Run)
  , XPosition(..)
  )
import qualified Xmobar as Bar (Config(..))

data Xmobar =
  Xmobar
    { readWM :: Maybe ScreenId
    , font :: Font
    , colorScheme :: ColorScheme
    , normalPP :: PP
    , focusedPP :: PP
    , additionalFonts :: [Font]
    , wmClass :: String
    , wmName :: String
    , position :: XPosition
    , textOffset :: Int
    , textOffsets :: [Int]
    , iconOffset :: Int
    , border :: Border
    , borderWidth :: Int
    , alpha :: Int
    , hideOnStart :: Bool
    , allDesktops :: Bool
    , overrideRedirect :: Bool
    , pickBroadest :: Bool
    , lowerOnStart :: Bool
    , persistent :: Bool
    , iconRoot :: FilePath
    , commands :: [Runnable]
    , sepChar :: String
    , alignSep :: String
    , template :: String
    , verbose :: Bool
    }

newtype WMReader =
  WMReader (Chan String)

instance Show WMReader where
  show _ = "WMReader"

instance Read WMReader where
  readsPrec = undefined

instance Exec WMReader where
  alias = show
  start (WMReader chan) cb = forever $ readChan chan >>= cb

xmobarToConfig :: Xmobar -> Chan String -> Config
xmobarToConfig c r =
  Config
    { Bar.font = getXft $ font c
    , Bar.additionalFonts = map getXft $ additionalFonts c
    , Bar.wmClass = wmClass c
    , Bar.wmName = wmName c
    , Bar.bgColor = base01 $ colorScheme c
    , Bar.fgColor = base05 $ colorScheme c
    , Bar.alpha = alpha c
    , Bar.position = position c
    , Bar.border = border c
    , Bar.borderColor = base03 $ colorScheme c
    , Bar.borderWidth = borderWidth c
    , Bar.textOffset = textOffset c
    , Bar.iconOffset = iconOffset c
    , Bar.textOffsets = textOffsets c
    , Bar.hideOnStart = hideOnStart c
    , Bar.lowerOnStart = lowerOnStart c
    , Bar.persistent = persistent c
    , Bar.allDesktops = allDesktops c
    , Bar.overrideRedirect = overrideRedirect c
    , Bar.pickBroadest = pickBroadest c
    , Bar.iconRoot = iconRoot c
    , Bar.commands = Run (WMReader r) : commands c
    , Bar.sepChar = sepChar c
    , Bar.alignSep = alignSep c
    , Bar.template = template c
    , Bar.verbose = verbose c
    }

xmobarDefaults :: Xmobar
xmobarDefaults =
  Xmobar
    { readWM = Nothing
    , font = fontDefaults
    , colorScheme = snazzyCS
    , normalPP = xmobarPP
    , focusedPP = xmobarPP
    , additionalFonts = []
    , wmClass = "xmobar"
    , wmName = "xmobar"
    , position = Top
    , textOffset = -1
    , textOffsets = []
    , iconOffset = -1
    , border = NoBorder
    , borderWidth = 1
    , alpha = 255
    , hideOnStart = False
    , allDesktops = True
    , overrideRedirect = True
    , pickBroadest = False
    , lowerOnStart = False
    , persistent = False
    , iconRoot = "."
    , commands = []
    , sepChar = "%"
    , alignSep = "}{"
    , template = ""
    , verbose = False
    }

screen :: ScreenId -> Xmobar -> XPosition
screen (S s) c = OnScreen s $ position c
