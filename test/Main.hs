module Main where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder.Extra as ByteStringBuilderExtra
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Vector as Vector
import qualified MooreMachines as Mm
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances
import qualified Test.QuickCheck.Property as QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import Prelude

main =
  defaultMain $
    testGroup "All" $
      [ testProperty "feedingFoldable" $ \(input :: [Int]) ->
          input
            === extract (Mm.feedingFoldable input ask),
        testProperty "charText" $ \(input :: [Char]) ->
          Text.pack input
            === extract (Mm.feedingFoldable input Mm.charText),
        testProperty "feedingTextChars" $ \(input :: Text) ->
          input
            === extract (Mm.feedingTextChars input Mm.charText),
        testGroup "trimmingWhitespace" $
          [ testProperty "1" $ \(text :: Text) ->
              let words =
                    Text.words text
                  spacedInput =
                    Text.map (\c -> if isSpace c then ' ' else c) text
                  newlinedInput =
                    Text.map (\c -> if isSpace c then '\n' else c) text
                  process text =
                    extract (Mm.feedingFoldable (Text.unpack text) (Mm.trimmingWhitespace Mm.charText))
               in Text.unwords words === process spacedInput
                    .&&. Text.intercalate "\n" words === process newlinedInput,
            testProperty "2" $ \(text :: Text) ->
              let isNewline c =
                    c == '\n' || c == '\r'
                  isSpaceButNotNewline c =
                    isSpace c && not (isNewline c)
                  normalize separator condition =
                    Text.split condition
                      >>> filter (not . Text.null)
                      >>> Text.intercalate separator
                  expected =
                    text
                      & Text.split isNewline
                      & fmap Text.strip
                      & filter (not . Text.null)
                      & Text.intercalate "\n"
                      & Text.split isSpaceButNotNewline
                      & filter (not . Text.null)
                      & Text.intercalate " "
                  actual =
                    extract (Mm.feedingFoldable (Text.unpack text) (Mm.trimmingWhitespace Mm.charText))
               in expected === actual
          ],
        testGroup "foldMany" $
          [ testProperty "" $ \(input :: Text) ->
              let actual =
                    Atto.parseOnly (Mm.foldMany ask (Atto.satisfy isAlphaNum)) input
                  expected =
                    Atto.parseOnly (many (Atto.satisfy isAlphaNum)) input
               in expected === actual
          ],
        testGroup "feedingMany" $
          [ testProperty "" $ \(input :: Text) ->
              let actual =
                    Atto.parseOnly (Mm.feedingMany ask (Atto.satisfy isAlphaNum)) input
                      & fmap extract
                  expected =
                    Atto.parseOnly (many (Atto.satisfy isAlphaNum)) input
               in expected === actual
          ],
        testGroup "concat" $
          [ testProperty "" $ \(list :: [[Int]]) ->
              concat list
                === extract (Mm.feedingFoldable list Mm.concat)
          ],
        testGroup "count" $
          [ testProperty "" $ \(list :: [Int]) ->
              length list
                === extract (Mm.feedingFoldable list Mm.count)
          ],
        testGroup "byteString" $
          [ testProperty "" $ \(list :: [Word8]) ->
              ByteString.pack list
                === extract (Mm.feedingFoldable list Mm.byteString)
          ],
        testGroup "vector" $
          [ testProperty "" $ \(list :: [Int]) ->
              fromList @(Vector.Vector Int) list
                === extract (Mm.feedingFoldable list Mm.vector)
          ],
        testGroup "sum" $
          [ testProperty "" $ \(list :: [Int]) ->
              sum list
                === extract (Mm.feedingFoldable list Mm.sum)
          ],
        testGroup "textText" $
          [ testProperty "" $ \(input :: [Text]) ->
              mconcat input
                === extract (Mm.feedingFoldable input Mm.textText)
          ],
        testGroup "decodingUtf8" $
          [ testProperty "Valid data" $ \(input :: Text) ->
              let binaryChunks =
                    input
                      & Text.encodeUtf8Builder
                      & ByteStringBuilderExtra.toLazyByteStringWith
                        (ByteStringBuilderExtra.safeStrategy 3 9)
                        ""
                      & ByteStringLazy.toChunks
               in Right input
                    === extract (Mm.feedingFoldable binaryChunks (Mm.decodingUtf8 Mm.textText)),
            testProperty "Any data" $
              QuickCheck.verbose $ \(input :: [ByteString]) ->
                first (const ()) (TextEncoding.decodeUtf8' (mconcat input))
                  === first (const ()) (extract (Mm.feedingFoldable input (Mm.decodingUtf8 Mm.textText)))
          ],
        testGroup "foldingTextChars" $
          [ testProperty "" $ \(input :: [Text]) ->
              Text.unpack (mconcat input)
                === extract (Mm.feedingFoldable input (Mm.foldingTextChars ask))
          ]
      ]
