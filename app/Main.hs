module Main (main) where

import KeenWM.Main (keenwm)
import KeenWM.Config (Default(..))

main :: IO ()
main = keenwm id def
