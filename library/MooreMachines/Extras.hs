module MooreMachines.Extras
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


{-|
Same as 'many', but immediately folds the value.
-}
foldMany :: MonadPlus f => Moore a b -> f a -> f b
foldMany machine fa =
  let
    loop (Moore terminate progress) =
      (fa >>= loop . progress) <|>
      pure terminate
    in loop machine

{-|
Update machine by feeding one input to it.
-}
{-# INLINE feeding #-}
feeding :: a -> Moore a output -> Moore a output
feeding input (Moore _ progress) =
  progress input

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

{-|
Transformer of chars,
replaces all space-like chars with space,
all newline-like chars with @\\n@,
and trims their duplicate sequences to single-char.
Oh yeah, it also trims whitespace from beginning and end.
-}
trimmingWhitespace :: Moore Char output -> Moore Char output
trimmingWhitespace =
  loop False False False
  where
    loop notFirst spacePending newlinePending machine =
      Moore terminate progress
      where
        progress char =
          if isSpace char
            then if char == '\n' || char == '\r'
              then loop notFirst False True machine
              else loop notFirst True newlinePending machine
            else
              let
                mapper =
                  if notFirst
                    then if newlinePending
                      then feeding '\n'
                      else if spacePending
                        then feeding ' '
                        else id
                    else id
                in
                  loop True False False $ feeding char $ mapper $ machine
        terminate =
          extract machine

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
