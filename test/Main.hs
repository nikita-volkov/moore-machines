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
import qualified MooreMachines as Mm


main =
  defaultMain $ testGroup "All" $ [
    testProperty "feedingFoldable" $ \(input :: [Int]) ->
      input ===
      extract (Mm.feedingFoldable input ask)
    ,
    testProperty "charText" $ \(input :: [Char]) ->
      Text.pack input ===
      extract (Mm.feedingFoldable input Mm.charText)
    ,
    testProperty "feedingTextChars" $ \(input :: Text) ->
      input ===
      extract (Mm.feedingTextChars input Mm.charText)
    ]
