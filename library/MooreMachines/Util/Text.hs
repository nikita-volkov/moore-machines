module MooreMachines.Util.Text
where

import MooreMachines.Prelude
import qualified Data.Text.Internal as TextInternal
import qualified Data.Text as Text
import qualified MooreMachines.Util.TextArray as TextArrayUtil


fromReverseListOfBytes :: Int -> [Word16] -> Text
fromReverseListOfBytes arraySize revListOfBytes =
  TextInternal.Text (TextArrayUtil.fromReverseListOfBytes arraySize revListOfBytes) 0 arraySize
