module Main
  ( main
  ) where

import KeenWM.Config (Default(..))
import KeenWM.Main (keenwm)

main :: IO ()
main = keenwm id def
