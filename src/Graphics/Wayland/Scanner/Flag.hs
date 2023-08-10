module Graphics.Wayland.Scanner.Flag (
  Flag (..),
  Flags,
  makeFlags,
  hasFlag,
  toFlags,
  fromFlags,
) where

import Data.Bits (Bits (..))
import Data.Coerce
import Data.List

-- | Denotes a type encoded as a bit field.
class Flag a where
  flagBits :: a -> Word

-- | Type denoting a bitfield of flags.
newtype Flags a = InternalMkFlag Word -- Encoded as a phantom type now.

-- Placeholder for now, need to think about this.
instance Show (Flags a) where
  show :: Flags a -> String
  show _ = "Flags"

makeFlags :: (Flag a) => [a] -> Flags a
makeFlags flags = toFlags . foldl' (.|.) 0 $ flagBits <$> flags

hasFlag :: (Flag a) => a -> Flags a -> Bool
hasFlag tar (InternalMkFlag bitfield) = toTest .&. bitfield == toTest
 where
  toTest = flagBits tar

toFlags :: (Flag a) => Word -> Flags a
toFlags = coerce

fromFlags :: (Flag a) => Flags a -> Word
fromFlags = coerce
