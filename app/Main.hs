module Main (main) where

import KeenWM.Main (keenwm)
import KeenWM.Config (kconfigDefaults)

main :: IO ()
main = keenwm kconfigDefaults id
