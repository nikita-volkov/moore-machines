module MooreMachines.Util.Text where

import qualified Data.Text as Text
import qualified Data.Text.Internal as TextInternal
import MooreMachines.Prelude
import qualified MooreMachines.Util.TextArray as TextArrayUtil

fromReverseListOfBytes :: Int -> [Word16] -> Text
fromReverseListOfBytes arraySize revListOfBytes =
  TextInternal.Text (TextArrayUtil.fromReverseListOfBytes arraySize revListOfBytes) 0 arraySize

fromReverseListOfTexts :: Int -> [Text] -> Text
fromReverseListOfTexts size list =
  TextInternal.Text (TextArrayUtil.fromReverseListOfTexts size list) 0 size
