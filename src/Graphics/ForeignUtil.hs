module Graphics.ForeignUtil (
  withNullPtr,
  nullCk,
  peekDouble,
  toFlags,
  fromFlags,
)
where

import Control.Monad
import Data.Coerce
import Data.Foldable
import Data.Set qualified as S
import Foreign
import Foreign.C.Types

withNullPtr :: (Ptr () -> IO a) -> IO a
withNullPtr f = f nullPtr

nullCk :: (Coercible (Ptr a) b) => b -> Maybe b
nullCk = coerce $ \ptr ->
  if ptr == nullPtr then Nothing else Just ptr

peekDouble :: Ptr CDouble -> IO Double
peekDouble = coerce <$> peek

{-# INLINE toFlags #-}
toFlags :: (Integral n, Bits n, Bounded a, Enum a, Ord a) => n -> S.Set a
toFlags bits = S.fromList $ do
  flag <- [minBound .. maxBound]
  guard $ (fromIntegral . fromEnum) flag .&. bits /= 0
  pure flag

{-# INLINE fromFlags #-}
fromFlags :: (Num n, Bits n, Enum a) => S.Set a -> n
fromFlags flags = foldl' (.|.) zeroBits $ fromIntegral . fromEnum <$> S.toList flags
