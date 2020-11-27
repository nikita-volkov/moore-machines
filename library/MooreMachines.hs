module MooreMachines
where

import MooreMachines.Prelude
import Data.Machine.Moore
import qualified Data.Text.Internal as TextInternal
import qualified Data.Text.Internal.Encoding.Utf16 as TextUtf16
import qualified Data.Text.Internal.Private as TextPrivate
import qualified Data.Text.Internal.Unsafe.Char as TextChar
import qualified Data.Text.Array as TextArray
import qualified Data.Text as Text
import qualified MooreMachines.Util.TextArray as TextArrayUtil


feedFoldable :: Foldable f => f a -> Moore a b -> Moore a b
feedFoldable foldable moore =
  foldr progress finalize foldable moore
  where
    progress a next (Moore terminate progress) =
      next (progress a)
    finalize =
      id

feedTextChars :: Text -> Moore Char o -> Moore Char o
feedTextChars (TextInternal.Text arr off len) =
  loop off
  where
    loop !off (Moore terminate progress) =
      if off >= len
        then
          Moore terminate progress
        else
          TextArrayUtil.iter arr off $ \ char newOff ->
            loop newOff (progress char)
