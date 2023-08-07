module Graphics.Wayland.Scanner.Flag (
  Flag (..),
  Flags,
  hasFlag,
  toFlags,
  fromFlags,
) where

import Data.Bits (Bits (..))
import Data.Coerce

-- | Denotes a type encoded as a bit field.
class Flag a where
  flagBits :: a -> Word

-- | Type denoting a bitfield of flags.
newtype Flags a = InternalMkFlag Word -- Encoded as a phantom type now.

hasFlag :: (Flag a) => a -> Flags a -> Bool
hasFlag tar (InternalMkFlag bitfield) = toTest .&. bitfield == toTest
 where
  toTest = flagBits tar

toFlags :: (Flag a) => Word -> Flags a
toFlags = coerce

fromFlags :: (Flag a) => Flags a -> Word
fromFlags = coerce
