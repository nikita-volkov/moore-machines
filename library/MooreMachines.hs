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


feedingFoldable :: Foldable f => f a -> Moore a b -> Moore a b
feedingFoldable foldable moore =
  foldr progress finalize foldable moore
  where
    progress a next (Moore terminate progress) =
      next (progress a)
    finalize =
      id

feedingTextChars :: Text -> Moore Char output -> Moore Char output
feedingTextChars (TextInternal.Text arr off len) =
  loop off
  where
    loop !off (Moore terminate progress) =
      if off >= len
        then
          Moore terminate progress
        else
          TextArrayUtil.iter arr off $ \ char newOff ->
            loop newOff (progress char)

charText :: Moore Char Text
charText =
  next [] 0
  where
    next bytes arraySize =
      Moore (terminate bytes arraySize) (progress bytes arraySize)
    progress !bytes !arraySize char =
      let
        codepoint =
          ord char
        in if codepoint < 0x10000
          then
            next (fromIntegral codepoint : bytes) (succ arraySize)
          else let
            cpBasis =
              codepoint - 0x10000
            byte1 =
              fromIntegral ((cpBasis `shiftR` 10) + 0xd800)
            byte2 =
              fromIntegral ((cpBasis .&. 0x3ff) + 0xdc00)
            in next (byte2 : byte1 : bytes) (arraySize + 2)
    terminate revListOfBytes arraySize =
      TextPrivate.runText $ \freeze -> do
        array <- TextArray.new arraySize
        let
          loop !offset bytes =
            case bytes of
              byte : bytesTail ->
                TextArray.unsafeWrite array offset byte *>
                loop (pred offset) bytesTail
              [] ->
                return ()
          in loop (pred arraySize) revListOfBytes
        freeze array arraySize
