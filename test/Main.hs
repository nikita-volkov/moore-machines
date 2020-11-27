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
    ,
    testGroup "trimmingWhitespace" [
      testProperty "1" $ \ (text :: Text) ->
        let
          words =
            Text.words text
          spacedInput =
            Text.map (\ c -> if isSpace c then ' ' else c) text
          newlinedInput =
            Text.map (\ c -> if isSpace c then '\n' else c) text
          process text =
            extract (Mm.feedingFoldable (Text.unpack text) (Mm.trimmingWhitespace Mm.charText))
          in
            Text.unwords words === process spacedInput .&&.
            Text.intercalate "\n" words === process newlinedInput
      ,
      testProperty "2" $ \ (text :: Text) ->
        let
          isNewline c =
            c == '\n' || c == '\r'
          isSpaceButNotNewline c =
            isSpace c && not (isNewline c)
          normalize separator condition =
            Text.split condition >>>
            filter (not . Text.null) >>>
            Text.intercalate separator
          expected =
            text &
            Text.split isNewline &
            fmap Text.strip &
            filter (not . Text.null) &
            Text.intercalate "\n" &
            Text.split isSpaceButNotNewline &
            filter (not . Text.null) &
            Text.intercalate " "
          actual =
            extract (Mm.feedingFoldable (Text.unpack text) (Mm.trimmingWhitespace Mm.charText))
          in
            expected === actual
      ]
    ]
