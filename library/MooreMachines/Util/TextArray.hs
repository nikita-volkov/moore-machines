module MooreMachines.Util.TextArray where

import Data.Text.Array
import qualified Data.Text.Internal as TextInternal
import qualified Data.Text.Internal.Encoding.Utf16 as TextUtf16
import qualified Data.Text.Internal.Unsafe.Char as TextChar
import MooreMachines.Prelude hiding (Array)

-- |
-- Same as 'Data.Text.Unsafe.iter',
-- but operates on the array directly,
-- uses a continuation and passes the next offset to it instead of delta.
{-# INLINE iter #-}
iter :: Array -> Int -> (Char -> Int -> a) -> a
iter arr offset cont =
  let b1 =
        unsafeIndex arr offset
   in if b1 >= 0xd800 && b1 <= 0xdbff
        then
          let b2 =
                unsafeIndex arr (succ offset)
              char =
                TextUtf16.chr2 b1 b2
           in cont char (offset + 2)
        else cont (TextChar.unsafeChr b1) (offset + 1)

fromReverseListOfBytes :: Int -> [Word16] -> Array
fromReverseListOfBytes arraySize revListOfBytes =
  runST $ do
    array <- new arraySize
    let loop !offset bytes =
          case bytes of
            byte : bytesTail ->
              unsafeWrite array offset byte
                *> loop (pred offset) bytesTail
            [] ->
              unsafeFreeze array
     in loop (pred arraySize) revListOfBytes

fromReverseListOfTexts :: Int -> [Text] -> Array
fromReverseListOfTexts size list =
  runST $ do
    array <- new size
    let loop offset =
          \case
            TextInternal.Text chunkArray chunkOffset chunkSize : chunks ->
              let !newOffset =
                    offset - chunkSize
               in copyI array newOffset chunkArray chunkOffset offset
                    *> loop newOffset chunks
            _ ->
              unsafeFreeze array
     in loop size list
