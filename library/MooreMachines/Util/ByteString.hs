module MooreMachines.Util.ByteString where

import Data.ByteString
import Data.ByteString.Internal
import MooreMachines.Prelude

fromReverseList :: Int -> [Word8] -> ByteString
fromReverseList size list =
  unsafeCreate size $ \p -> loop (plusPtr p (pred size)) list
  where
    loop !p =
      \case
        [] -> return ()
        h : t -> poke p h >> loop (plusPtr p (-1)) t
