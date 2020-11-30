module MooreMachines.Extras
where

import MooreMachines.Prelude
import Data.Machine.Moore
import qualified Data.Text.Internal as TextInternal
import qualified Data.Text as Text
import qualified MooreMachines.Util.Char as CharUtil
import qualified MooreMachines.Util.Text as TextUtil
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
feedingFoldable =
  foldr step id
  where
    step a next !moore =
      next (feeding a moore)

feedingIndexable :: (Int -> indexable -> a) -> Int -> Int -> indexable -> Moore a b -> Moore a b
feedingIndexable getElement startOff afterEndOff indexable =
  loop startOff
  where
    loop !off !machine =
      if off < afterEndOff
        then loop (succ off) (feeding (getElement off indexable) machine)
        else machine

feedingTextChars :: Text -> Moore Char output -> Moore Char output
feedingTextChars (TextInternal.Text arr off len) =
  loop off
  where
    loop !off !moore =
      if off >= len
        then
          moore
        else
          TextArrayUtil.iter arr off $ \ char newOff ->
            loop newOff (feeding char moore)

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
    next !bytes !arraySize =
      Moore terminate progress
      where
        progress =
          CharUtil.encodeInUtf16
            (\byte -> next (byte : bytes) (succ arraySize))
            (\byte1 byte2 -> next (byte2 : byte1 : bytes) (arraySize + 2))
        terminate =
          TextUtil.fromReverseListOfBytes arraySize bytes
