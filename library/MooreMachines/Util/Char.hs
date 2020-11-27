module MooreMachines.Util.Char
where

import MooreMachines.Prelude


{-# INLINE encodeInUtf16 #-}
encodeInUtf16 :: (Word16 -> a) -> (Word16 -> Word16 -> a) -> Char -> a
encodeInUtf16 singleCont doubleCont char =
  let
    codepoint =
      ord char
    in if codepoint < 0x10000
      then
        singleCont (fromIntegral codepoint)
      else let
        cpBasis =
          codepoint - 0x10000
        byte1 =
          fromIntegral ((cpBasis `shiftR` 10) + 0xd800)
        byte2 =
          fromIntegral ((cpBasis .&. 0x3ff) + 0xdc00)
        in doubleCont byte1 byte2
