module Types (
  BB
, MagicEntry (MagicEntry, mask, magic, bitshift, lookupTable)
) where

import Data.Word
import Data.Array.Unboxed
import Control.DeepSeq


-- bitboard
type BB = Word64

data MagicEntry = MagicEntry { mask :: BB
							 , magic :: BB
							 , bitshift :: Int
							 , lookupTable :: UArray Int BB
                  } deriving (Show)
 