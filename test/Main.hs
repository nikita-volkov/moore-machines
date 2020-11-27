module Main where

import Prelude
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Property as QuickCheck
import qualified Data.Text as Text
import qualified Data.Attoparsec.Text as Atto
import qualified MooreMachines


main =
  defaultMain $ testGroup "All" $ [
    
    ]
