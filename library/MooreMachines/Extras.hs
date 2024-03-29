module MooreMachines.Extras where

import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict as HashMap
import Data.Machine.Moore
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import qualified Data.Text.Internal as TextInternal
import qualified Data.Text.Unsafe as TextUnsafe
import qualified Data.Vector.Generic as GenericVector
import MooreMachines.Prelude hiding (sum)
import qualified MooreMachines.Util.ByteString as ByteStringUtil
import qualified MooreMachines.Util.Char as CharUtil
import qualified MooreMachines.Util.Text as TextUtil
import qualified MooreMachines.Util.TextArray as TextArrayUtil
import qualified MooreMachines.Util.Vector as VectorUtil

-- |
-- Same as 'many', but immediately folds the value.
foldMany :: MonadPlus f => Moore a b -> f a -> f b
foldMany machine fa =
  let loop (Moore terminate progress) =
        (fa >>= loop . progress)
          <|> pure terminate
   in loop machine

-- |
-- Same as 'foldMany', but produces a machine,
-- which you can continue feeding afterwards or extract the result from it.
feedingMany :: MonadPlus f => Moore a b -> f a -> f (Moore a b)
feedingMany machine fa =
  let loop machine =
        (fa >>= loop . flip feeding machine)
          <|> pure machine
   in loop machine

-- |
-- Update machine by feeding one input to it.
{-# INLINE feeding #-}
feeding :: a -> Moore a output -> Moore a output
feeding input (Moore _ progress) =
  progress input

-- |
-- Update machine by feeding all elements of a foldable to it.
feedingFoldable :: Foldable f => f a -> Moore a b -> Moore a b
feedingFoldable =
  foldr step id
  where
    step a next !moore =
      next (feeding a moore)

-- |
-- A low-level helper allowing to feed indexing-optimized data-structures,
-- such as 'Vector'.
feedingIndexable :: (Int -> indexable -> a) -> Int -> Int -> indexable -> Moore a b -> Moore a b
feedingIndexable getElement startOff afterEndOff indexable =
  loop startOff
  where
    loop !off !machine =
      if off < afterEndOff
        then loop (succ off) (feeding (getElement off indexable) machine)
        else machine

feedingVector :: GenericVector.Vector v a => v a -> Moore a b -> Moore a b
feedingVector vec =
  feedingIndexable (flip GenericVector.unsafeIndex) 0 (GenericVector.length vec) vec

feedingTextChars :: Text -> Moore Char output -> Moore Char output
feedingTextChars (TextInternal.Text arr off len) =
  loop off
  where
    loop !off !moore =
      if off >= len
        then moore
        else TextArrayUtil.iter arr off $ \char newOff ->
          loop newOff (feeding char moore)

folder :: (a -> b -> b) -> b -> Moore a b
folder step =
  loop
  where
    loop !acc =
      Moore acc progress
      where
        progress input =
          loop (step input acc)

foldingWithStep :: (forall o. i2 -> Moore i1 o -> Moore i1 o) -> Moore i1 o -> Moore i2 o
foldingWithStep step =
  fmap extract . folder step

foldingTextChars :: Moore Char a -> Moore Text a
foldingTextChars =
  foldingWithStep feedingTextChars

mapInput :: (a -> b) -> Moore b o -> Moore a o
mapInput map (Moore terminate progress) =
  Moore terminate (mapInput map . progress . map)

mapProgress ::
  (forall o. (b -> Moore b o) -> (a -> Moore a o)) ->
  (forall o. Moore b o -> Moore a o)
mapProgress map (Moore terminate progress) =
  Moore terminate (map progress)

-- |
-- Transformer of chars,
-- replaces all space-like chars with space,
-- all newline-like chars with @\\n@,
-- and trims their duplicate sequences to single-char.
-- Oh yeah, it also trims whitespace from beginning and end.
trimmingWhitespace :: Moore Char output -> Moore Char output
trimmingWhitespace =
  loop False False False
  where
    loop notFirst spacePending newlinePending !machine =
      Moore terminate progress
      where
        progress char =
          if isSpace char
            then
              if char == '\n' || char == '\r'
                then loop notFirst False True machine
                else loop notFirst True newlinePending machine
            else
              let mapper =
                    if notFirst
                      then
                        if newlinePending
                          then feeding '\n'
                          else
                            if spacePending
                              then feeding ' '
                              else id
                      else id
               in loop True False False $ feeding char $ mapper $ machine
        terminate =
          extract machine

-- |
-- Efficiently construct text from chars.
--
-- During the feeding phase aggregates the chars into a list of UTF-16 encoded bytes in reverse order and
-- counts their amount.
-- During the extraction allocates the accordingly sized array and
-- populates it with bytes from the list in reverse order.
charText :: Moore Char Text
charText =
  next [] 0
  where
    next !bytes !arraySize =
      Moore terminate progress
      where
        progress =
          CharUtil.encodeInUtf16
            (\(!byte) -> next (byte : bytes) (succ arraySize))
            (\(!byte1) (!byte2) -> next (byte2 : byte1 : bytes) (arraySize + 2))
        terminate =
          TextUtil.fromReverseListOfBytes arraySize bytes

-- |
-- Efficiently concatenate texts.
textText :: Moore Text Text
textText =
  TextUtil.fromReverseListOfTexts <$> lmap TextUnsafe.lengthWord16 sum <*> revList

revList :: Moore a [a]
revList =
  loop []
  where
    loop !list =
      Moore list (\a -> loop (a : list))

byteString :: Moore Word8 ByteString
byteString =
  ByteStringUtil.fromReverseList <$> count <*> revList

vector :: GenericVector.Vector v a => Moore a (v a)
vector =
  VectorUtil.fromReverseListN <$> count <*> revList

count :: Moore a Int
count =
  loop 0
  where
    loop !count =
      Moore count (const (loop (succ count)))

sum :: Num a => Moore a a
sum =
  loop 0
  where
    loop !a =
      Moore a (\b -> loop (a + b))

concat :: Monoid a => Moore a a
concat =
  loop mempty
  where
    loop !acc =
      Moore acc (\input -> loop (acc <> input))

countEach :: (Eq a, Hashable a) => Moore a (HashMap a Int)
countEach =
  loop HashMap.empty
  where
    loop !a =
      Moore a (\b -> loop (HashMap.alter alterer b a))
    alterer =
      Just . maybe 1 succ

-- |
-- Upgrade a machine which reduces text chunks
-- to a machine which decodes binary data chunks,
-- possibly failing and exposing the failed binary chunk in the result.
decodingUtf8 :: Moore Text a -> Moore ByteString (Either ByteString a)
decodingUtf8 =
  unfinished "" TextEncoding.streamDecodeUtf8
  where
    unfinished remainder decoder !moore =
      Moore terminate progress
      where
        terminate =
          if ByteString.null remainder
            then Right (extract moore)
            else Left remainder
        progress input =
          catchPureException fromDecoding fromException (decoder input)
          where
            fromDecoding (TextEncoding.Some decodedChunk remainder newDecoder) =
              unfinished remainder newDecoder (feeding decodedChunk moore)
            fromException (TextEncodingError.DecodeError _ _) =
              pure (Left input)
